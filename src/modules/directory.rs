#[cfg(not(target_os = "windows"))]
use super::utils::directory_nix as directory_utils;
#[cfg(target_os = "windows")]
use super::utils::directory_win as directory_utils;
use super::utils::path::{NormalizedPath, PathExt, PathKind};
use std::fmt::Write;
use std::path::Path;
use unicode_segmentation::UnicodeSegmentation;

use super::{Context, Module};

use crate::config::RootModuleConfig;
use crate::configs::directory::{DirectoryConfig, PathSeparatorOption};
use crate::formatter::StringFormatter;

/// Creates a module with the current logical or physical directory
///
/// Will perform path contraction, substitution, and truncation.
///
/// **Contraction**
/// - Paths beginning with the home directory or with a git repo right inside
///   the home directory will be contracted to `~`
/// - Paths containing a git repo will contract to begin at the repo root
///
/// **Substitution**
/// Paths will undergo user-provided substitutions of substrings
///
/// **Truncation**
/// Paths will be limited in length to `3` path components by default.
pub fn module<'a>(context: &'a Context) -> Option<Module<'a>> {
    let mut module = context.new_module("directory");
    let config: DirectoryConfig = DirectoryConfig::try_load(module.config);

    let home_dir = dirs_next::home_dir().expect("Unable to determine HOME_DIR for user");
    let home_dir = home_dir.normalize();

    let physical_dir = &context.current_dir;
    let current_dir = if config.use_logical_path {
        &context.logical_dir
    } else {
        &context.current_dir
    };

    log::debug!("Home dir: {:?}", &home_dir);
    log::debug!("Physical dir: {:?}", &physical_dir);
    log::debug!("Current dir: {:?}", &current_dir);

    // Attempt repository path contraction (if we are in a git repository)
    let repo = if config.truncate_to_repo {
        context.get_repo().ok()
    } else {
        None
    };

    let display_path = repo
        .and_then(|repo| repo.root.as_ref())
        .and_then(|repo_root| {
            let repo_root = repo_root.normalize();
            // If the user's home repo is a git directory then
            // prefer normal home path contraction.
            if repo_root == home_dir {
                return None;
            }
            // NOTE: Always attempt to contract repo paths from the physical dir as
            // the logical dir _may_ not be be a valid physical disk
            // path and may not include the repo path prefix.
            contract_repo_path(physical_dir.normalize(), &repo_root)
        })
        .unwrap_or_else(|| {
            // Fall back to the logical path, automatically contracting
            // the home directory if required.
            contract_home_path(current_dir.normalize(), &home_dir, &config)
        });

    // Apply path substitutions
    let display_path = substitute_path(display_path, &config);

    let formatted_path = format_path_for_display(&display_path, &config).expect("formatted path");
    let lock_symbol = String::from(config.read_only);

    let parsed = StringFormatter::new(config.format).and_then(|formatter| {
        formatter
            .map_style(|variable| match variable {
                "style" => Some(Ok(config.style)),
                "read_only_style" => Some(Ok(config.read_only_style)),
                _ => None,
            })
            .map(|variable| match variable {
                "path" => Some(Ok(&formatted_path)),
                "read_only" => {
                    if is_readonly_dir(&physical_dir) {
                        Some(Ok(&lock_symbol))
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .parse(None)
    });

    module.set_segments(match parsed {
        Ok(segments) => segments,
        Err(error) => {
            log::warn!("Error in module `directory`:\n{}", error);
            return None;
        }
    });

    Some(module)
}

fn is_readonly_dir(path: &Path) -> bool {
    match directory_utils::is_write_allowed(path) {
        Ok(res) => !res,
        Err(e) => {
            log::debug!(
                "Failed to determine read only status of directory '{:?}': {}",
                path,
                e
            );
            false
        }
    }
}

// Attempts to contract the path to the home path.
fn contract_home_path<'a>(
    mut path: NormalizedPath<'a>,
    home_path: &NormalizedPath,
    config: &DirectoryConfig<'a>,
) -> NormalizedPath<'a> {
    if !home_path.is_absolute() || !path.starts_with(home_path) {
        return path;
    }
    let short_home_path = Path::new(config.home_symbol).normalize();
    path.try_replace_sub_path(home_path, &short_home_path);
    path
}

// Attempts to contract the path to the given repository root.
fn contract_repo_path<'a>(
    mut path: NormalizedPath<'a>,
    repo_root: &NormalizedPath<'a>,
) -> Option<NormalizedPath<'a>> {
    if !repo_root.is_absolute() || !path.starts_with(repo_root) {
        return None;
    }
    // Replace the full repository path with a relative path
    // starting from the repo root
    let repo_name = repo_root.segments.iter().last().expect("repo name");
    let short_repo_path = Path::new(repo_name.as_ref()).normalize();
    path.try_replace_sub_path(repo_root, &short_repo_path);
    Some(path)
}

/// Apply the list of sub-path substitutions on the path.
///
/// Given a list of (from, to) pairs, this will perform the path
/// substitutions in order on the path.
///
/// Any non-pair of is ignored.
fn substitute_path<'a>(
    mut path: NormalizedPath<'a>,
    config: &DirectoryConfig,
) -> NormalizedPath<'a> {
    for (sub_path, replacement) in config.substitutions.iter() {
        let sub_path = Path::new(sub_path).normalize();
        let replacement = Path::new(replacement).normalize();
        path.try_replace_sub_path(&sub_path, &replacement);
    }
    path
}

/// Formats the path for final display.
///
/// Fish-style path segment shortening is applied by this routine.
fn format_path_for_display(
    path: &NormalizedPath,
    config: &DirectoryConfig,
) -> Result<String, std::fmt::Error> {
    let sep = match config.path_separator {
        PathSeparatorOption::Auto => {
            if cfg!(windows) {
                r"\"
            } else {
                r"/"
            }
        }
        PathSeparatorOption::Slash => r"/",
        PathSeparatorOption::Backslash => r"\",
    };
    let mut buf = String::new();
    // Are we applying path truncation?
    if config.truncation_length != 0 && config.truncation_length < path.segments.len() {
        // Write the truncation symbol if it is configured
        if !config.truncation_symbol.is_empty() {
            write!(buf, "{}", config.truncation_symbol)?;
        }
    } else {
        // Write the start of the path, if there is one.
        match path.kind {
            PathKind::Relative => {}
            PathKind::Absolute => {
                write!(buf, "{s}", s = sep)?;
            }
            PathKind::RelativeDrive(letter) => {
                write!(buf, "{l}:", l = letter)?;
            }
            PathKind::AbsoluteDrive(letter) => {
                write!(buf, "{l}:{s}", l = letter, s = sep)?;
            }
            PathKind::AbsoluteUnc => {
                write!(buf, "{s}{s}", s = sep)?;
            }
            PathKind::AbsoluteDevice => {
                write!(buf, "{s}{s}.{s}", s = sep)?;
            }
        }
    }
    // Fish-style path segment shortening
    let last_segment_index = path.segments.len().saturating_sub(1);
    let apply_fish_shortening = config.fish_style_pwd_dir_length > 0;
    // Path truncation
    let skip_segments = path.segments.len().saturating_sub(config.truncation_length);
    for (i, segment) in path.segments.iter().enumerate().skip(skip_segments) {
        let mut segment = segment.as_ref();
        // Fish-style path segment shortening
        // See: https://fishshell.com/docs/current/cmds/prompt_pwd.html
        if apply_fish_shortening && i != last_segment_index {
            segment = fish_shorten_path_segment(segment, config.fish_style_pwd_dir_length);
        }
        write!(buf, "{s}{p}", s = if i > 0 { sep } else { "" }, p = segment)?;
    }
    Ok(buf)
}

/// Applies fish-style path segment contraction to a segment
fn fish_shorten_path_segment(segment: &str, short_len: usize) -> &str {
    let mut graphemes = UnicodeSegmentation::grapheme_indices(segment, true);
    let end = match graphemes.next() {
        // Always include the leading period on .-prefixed
        // segments (., .., .hidden, etc)
        Some((_, ".")) => graphemes.take(short_len).last(),
        Some(_) => graphemes.take(short_len - 1).last(),
        None => None,
    };
    if let Some((end, _)) = end {
        return &segment[..=end];
    }
    segment
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test::ModuleRenderer;
    use ansi_term::Color;
    use dirs_next::home_dir;
    #[cfg(not(target_os = "windows"))]
    use std::os::unix::fs::symlink;
    #[cfg(target_os = "windows")]
    use std::os::windows::fs::symlink_dir as symlink;
    use std::path::Path;
    use std::process::Command;
    use std::{fs, io};
    use tempfile::TempDir;

    #[test]
    fn contract_home_directory() {
        let full_path = Path::new("/Users/astronaut/schematics/rocket");
        let home = Path::new("/Users/astronaut");

        let output = contract_path(full_path, home, "~");
        assert_eq!(output, "~/schematics/rocket");
    }

    #[test]
    fn contract_repo_directory() -> io::Result<()> {
        let tmp_dir = TempDir::new_in(home_dir().unwrap().as_path())?;
        let repo_dir = tmp_dir.path().join("dev").join("rocket-controls");
        let src_dir = repo_dir.join("src");
        fs::create_dir_all(&src_dir)?;
        init_repo(&repo_dir)?;

        let src_variations = [src_dir.clone(), src_dir.canonicalize().unwrap()];
        let repo_variations = [repo_dir.clone(), repo_dir.canonicalize().unwrap()];
        for src_dir in &src_variations {
            for repo_dir in &repo_variations {
                let output = contract_repo_path(&src_dir, &repo_dir);
                assert_eq!(output, Some("rocket-controls/src".to_string()));
            }
        }

        tmp_dir.close()
    }

    #[test]
    #[cfg(windows)]
    fn contract_windows_style_home_directory() {
        let path_variations = [
            r"\\?\C:\Users\astronaut\schematics\rocket",
            r"C:\Users\astronaut\schematics\rocket",
        ];
        let home_path_variations = [r"\\?\C:\Users\astronaut", r"C:\Users\astronaut"];
        for path in &path_variations {
            for home_path in &home_path_variations {
                let path = Path::new(path);
                let home_path = Path::new(home_path);

                let output = contract_path(path, home_path, "~");
                assert_eq!(output, "~/schematics/rocket");
            }
        }
    }

    #[test]
    #[cfg(target_os = "windows")]
    fn contract_windows_style_repo_directory() {
        let full_path = Path::new("C:\\Users\\astronaut\\dev\\rocket-controls\\src");
        let repo_root = Path::new("C:\\Users\\astronaut\\dev\\rocket-controls");

        let output = contract_path(full_path, repo_root, "rocket-controls");
        assert_eq!(output, "rocket-controls/src");
    }

    #[test]
    #[cfg(target_os = "windows")]
    fn contract_windows_style_no_top_level_directory() {
        let full_path = Path::new("C:\\Some\\Other\\Path");
        let top_level_path = Path::new("C:\\Users\\astronaut");

        let output = contract_path(full_path, top_level_path, "~");
        assert_eq!(output, "C:/Some/Other/Path");
    }

    #[test]
    #[cfg(target_os = "windows")]
    fn contract_windows_style_root_directory() {
        let full_path = Path::new("C:\\");
        let top_level_path = Path::new("C:\\Users\\astronaut");

        let output = contract_path(full_path, top_level_path, "~");
        assert_eq!(output, "C:");
    }

    #[test]
    fn substitute_prefix_and_middle() {
        let full_path = "/absolute/path/foo/bar/baz";
        let mut substitutions = IndexMap::new();
        substitutions.insert("/absolute/path".to_string(), "");
        substitutions.insert("/bar/".to_string(), "/");

        let output = substitute_path(full_path.to_string(), &substitutions);
        assert_eq!(output, "/foo/baz");
    }

    #[test]
    fn fish_style_with_user_home_contracted_path() {
        let path = "~/starship/engines/booster/rocket";
        let output = to_fish_style(1, path.to_string(), "engines/booster/rocket");
        assert_eq!(output, "~/s/");
    }

    #[test]
    fn fish_style_with_user_home_contracted_path_and_dot_dir() {
        let path = "~/.starship/engines/booster/rocket";
        let output = to_fish_style(1, path.to_string(), "engines/booster/rocket");
        assert_eq!(output, "~/.s/");
    }

    #[test]
    fn fish_style_with_no_contracted_path() {
        // `truncation_length = 2`
        let path = "/absolute/Path/not/in_a/repo/but_nested";
        let output = to_fish_style(1, path.to_string(), "repo/but_nested");
        assert_eq!(output, "/a/P/n/i/");
    }

    #[test]
    fn fish_style_with_pwd_dir_len_no_contracted_path() {
        // `truncation_length = 2`
        let path = "/absolute/Path/not/in_a/repo/but_nested";
        let output = to_fish_style(2, path.to_string(), "repo/but_nested");
        assert_eq!(output, "/ab/Pa/no/in/");
    }

    #[test]
    fn fish_style_with_duplicate_directories() {
        let path = "~/starship/tmp/C++/C++/C++";
        let output = to_fish_style(1, path.to_string(), "C++");
        assert_eq!(output, "~/s/t/C/C/");
    }

    #[test]
    fn fish_style_with_unicode() {
        let path = "~/starship/tmp/目录/a̐éö̲/目录";
        let output = to_fish_style(1, path.to_string(), "目录");
        assert_eq!(output, "~/s/t/目/a̐/");
    }

    fn init_repo(path: &Path) -> io::Result<()> {
        Command::new("git")
            .args(&["init"])
            .current_dir(path)
            .output()
            .map(|_| ())
    }

    fn make_known_tempdir(root: &Path) -> io::Result<(TempDir, String)> {
        fs::create_dir_all(root)?;
        let dir = TempDir::new_in(root)?;
        let path = dir
            .path()
            .file_name()
            .unwrap()
            .to_string_lossy()
            .to_string();
        Ok((dir, path))
    }

    #[cfg(not(target_os = "windows"))]
    mod linux {
        use super::*;
        use std::sync::atomic::{AtomicBool, Ordering};

        // As tests are run in parallel we have to keep a lock on which of the
        // two tests are currently running as they both modify `HOME` which can
        // override the other value resulting in inconsistent runs which is why
        // we only run one of these tests at once.
        static LOCK: AtomicBool = AtomicBool::new(false);

        #[test]
        #[ignore]
        fn symlinked_subdirectory_git_repo_out_of_tree() -> io::Result<()> {
            while LOCK.swap(true, Ordering::Acquire) {}
            let tmp_dir = TempDir::new_in(home_dir().unwrap().as_path())?;
            let repo_dir = tmp_dir.path().join("above-repo").join("rocket-controls");
            let src_dir = repo_dir.join("src/meters/fuel-gauge");
            let symlink_dir = tmp_dir.path().join("fuel-gauge");
            fs::create_dir_all(&src_dir)?;
            init_repo(&repo_dir)?;
            symlink(&src_dir, &symlink_dir)?;

            // We can't mock `HOME` since dirs-next uses it which does not care about our mocking
            let previous_home = home_dir().unwrap();

            std::env::set_var("HOME", tmp_dir.path());

            let actual = ModuleRenderer::new("directory").path(symlink_dir).collect();
            let expected = Some(format!("{} ", Color::Cyan.bold().paint("~/fuel-gauge")));

            std::env::set_var("HOME", previous_home.as_path());

            assert_eq!(expected, actual);

            LOCK.store(false, Ordering::Release);

            tmp_dir.close()
        }

        #[test]
        #[ignore]
        fn git_repo_in_home_directory_truncate_to_repo_true() -> io::Result<()> {
            while LOCK.swap(true, Ordering::Acquire) {}
            let tmp_dir = TempDir::new_in(home_dir().unwrap().as_path())?;
            let dir = tmp_dir.path().join("src/fuel-gauge");
            fs::create_dir_all(&dir)?;
            init_repo(&tmp_dir.path())?;

            // We can't mock `HOME` since dirs-next uses it which does not care about our mocking
            let previous_home = home_dir().unwrap();

            std::env::set_var("HOME", tmp_dir.path());

            let actual = ModuleRenderer::new("directory")
                .config(toml::toml! {
                    [directory]
                    // `truncate_to_repo = true` should attempt to display the truncated path
                    truncate_to_repo = true
                    truncation_length = 5
                })
                .path(dir)
                .collect();
            let expected = Some(format!("{} ", Color::Cyan.bold().paint("~/src/fuel-gauge")));

            std::env::set_var("HOME", previous_home.as_path());

            assert_eq!(expected, actual);

            LOCK.store(false, Ordering::Release);

            tmp_dir.close()
        }

        #[test]
        fn directory_in_root() -> io::Result<()> {
            let actual = ModuleRenderer::new("directory").path("/etc").collect();
            let expected = Some(format!(
                "{}{} ",
                Color::Cyan.bold().paint("/etc"),
                Color::Red.normal().paint("🔒")
            ));

            assert_eq!(expected, actual);
            Ok(())
        }
    }

    #[test]
    fn home_directory() -> io::Result<()> {
        let actual = ModuleRenderer::new("directory")
            .path(home_dir().unwrap())
            .config(toml::toml! { // Necessary if homedir is a git repo
                [directory]
                truncate_to_repo = false
            })
            .collect();
        let expected = Some(format!("{} ", Color::Cyan.bold().paint("~")));

        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn substituted_truncated_path() -> io::Result<()> {
        let actual = ModuleRenderer::new("directory")
            .path("/some/long/network/path/workspace/a/b/c/dev")
            .config(toml::toml! {
                [directory]
                truncation_length = 4
                [directory.substitutions]
                "/some/long/network/path" = "/some/net"
                "a/b/c" = "d"
            })
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("net/workspace/d/dev")
        ));

        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn substitution_order() -> io::Result<()> {
        let actual = ModuleRenderer::new("directory")
            .path("/path/to/sub")
            .config(toml::toml! {
                [directory.substitutions]
                "/path/to/sub" = "/correct/order"
                "/to/sub" = "/wrong/order"
            })
            .collect();
        let expected = Some(format!("{} ", Color::Cyan.bold().paint("/correct/order")));

        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn strange_substitution() -> io::Result<()> {
        let strange_sub = "/\\/;,!";
        let actual = ModuleRenderer::new("directory")
            .path("/foo/bar/regular/path")
            .config(toml::toml! {
                [directory]
                truncation_length = 0
                fish_style_pwd_dir_length = 2 // Overridden by substitutions
                [directory.substitutions]
                "regular" = strange_sub
            })
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan
                .bold()
                .paint(format!("/foo/bar/{}/path", strange_sub))
        ));

        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn directory_in_home() -> io::Result<()> {
        let (tmp_dir, name) = make_known_tempdir(home_dir().unwrap().as_path())?;
        let dir = tmp_dir.path().join("starship");
        fs::create_dir_all(&dir)?;

        let actual = ModuleRenderer::new("directory").path(dir).collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint(format!("~/{}/starship", name))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn truncated_directory_in_home() -> io::Result<()> {
        let (tmp_dir, name) = make_known_tempdir(home_dir().unwrap().as_path())?;
        let dir = tmp_dir.path().join("engine/schematics");
        fs::create_dir_all(&dir)?;

        let actual = ModuleRenderer::new("directory").path(dir).collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan
                .bold()
                .paint(format!("{}/engine/schematics", name))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn fish_directory_in_home() -> io::Result<()> {
        let (tmp_dir, name) = make_known_tempdir(home_dir().unwrap().as_path())?;
        let dir = tmp_dir.path().join("starship/schematics");
        fs::create_dir_all(&dir)?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 1
                fish_style_pwd_dir_length = 2
            })
            .path(&dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan
                .bold()
                .paint(format!("~/{}/st/schematics", name.split_at(3).0))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn root_directory() -> io::Result<()> {
        let actual = ModuleRenderer::new("directory").path("/").collect();
        #[cfg(not(target_os = "windows"))]
        let expected = Some(format!(
            "{}{} ",
            Color::Cyan.bold().paint("/"),
            Color::Red.normal().paint("🔒")
        ));
        #[cfg(target_os = "windows")]
        let expected = Some(format!("{} ", Color::Cyan.bold().paint("/")));

        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn truncated_directory_in_root() -> io::Result<()> {
        let (tmp_dir, name) = make_known_tempdir(Path::new("/tmp"))?;
        let dir = tmp_dir.path().join("thrusters/rocket");
        fs::create_dir_all(&dir)?;

        let actual = ModuleRenderer::new("directory").path(dir).collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan
                .bold()
                .paint(format!("{}/thrusters/rocket", name))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn truncated_directory_config_large() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let dir = tmp_dir.path().join("thrusters/rocket");
        fs::create_dir_all(&dir)?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 100
            })
            .path(&dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan
                .bold()
                .paint(truncate(dir.to_slash_lossy(), 100))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn fish_style_directory_config_large() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let dir = tmp_dir.path().join("thrusters/rocket");
        fs::create_dir_all(&dir)?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 1
                fish_style_pwd_dir_length = 100
            })
            .path(&dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan
                .bold()
                .paint(to_fish_style(100, dir.to_slash_lossy(), ""))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn truncated_directory_config_small() -> io::Result<()> {
        let (tmp_dir, name) = make_known_tempdir(Path::new("/tmp"))?;
        let dir = tmp_dir.path().join("rocket");
        fs::create_dir_all(&dir)?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 2
            })
            .path(dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint(format!("{}/rocket", name))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn fish_directory_config_small() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let dir = tmp_dir.path().join("thrusters/rocket");
        fs::create_dir_all(&dir)?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 2
                fish_style_pwd_dir_length = 1
            })
            .path(&dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint(format!(
                "{}/thrusters/rocket",
                to_fish_style(1, dir.to_slash_lossy(), "/thrusters/rocket")
            ))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn git_repo_root() -> io::Result<()> {
        let tmp_dir = TempDir::new()?;
        let repo_dir = tmp_dir.path().join("rocket-controls");
        fs::create_dir(&repo_dir)?;
        init_repo(&repo_dir).unwrap();

        let actual = ModuleRenderer::new("directory").path(repo_dir).collect();
        let expected = Some(format!("{} ", Color::Cyan.bold().paint("rocket-controls")));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn directory_in_git_repo() -> io::Result<()> {
        let tmp_dir = TempDir::new()?;
        let repo_dir = tmp_dir.path().join("rocket-controls");
        let dir = repo_dir.join("src");
        fs::create_dir_all(&dir)?;
        init_repo(&repo_dir).unwrap();

        let actual = ModuleRenderer::new("directory").path(dir).collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("rocket-controls/src")
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn truncated_directory_in_git_repo() -> io::Result<()> {
        let tmp_dir = TempDir::new()?;
        let repo_dir = tmp_dir.path().join("rocket-controls");
        let dir = repo_dir.join("src/meters/fuel-gauge");
        fs::create_dir_all(&dir)?;
        init_repo(&repo_dir).unwrap();

        let actual = ModuleRenderer::new("directory").path(dir).collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("src/meters/fuel-gauge")
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn directory_in_git_repo_truncate_to_repo_false() -> io::Result<()> {
        let tmp_dir = TempDir::new()?;
        let repo_dir = tmp_dir.path().join("above-repo").join("rocket-controls");
        let dir = repo_dir.join("src/meters/fuel-gauge");
        fs::create_dir_all(&dir)?;
        init_repo(&repo_dir).unwrap();

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                // Don't truncate the path at all.
                truncation_length = 5
                truncate_to_repo = false
            })
            .path(dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan
                .bold()
                .paint("above-repo/rocket-controls/src/meters/fuel-gauge")
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn fish_path_directory_in_git_repo_truncate_to_repo_false() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("above-repo").join("rocket-controls");
        let dir = repo_dir.join("src/meters/fuel-gauge");
        fs::create_dir_all(&dir)?;
        init_repo(&repo_dir).unwrap();

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                // Don't truncate the path at all.
                truncation_length = 5
                truncate_to_repo = false
                fish_style_pwd_dir_length = 1
            })
            .path(dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint(format!(
                "{}/above-repo/rocket-controls/src/meters/fuel-gauge",
                to_fish_style(1, tmp_dir.path().to_slash_lossy(), "")
            ))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn fish_path_directory_in_git_repo_truncate_to_repo_true() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("above-repo").join("rocket-controls");
        let dir = repo_dir.join("src/meters/fuel-gauge");
        fs::create_dir_all(&dir)?;
        init_repo(&repo_dir).unwrap();

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                // `truncate_to_repo = true` should display the truncated path
                truncation_length = 5
                truncate_to_repo = true
                fish_style_pwd_dir_length = 1
            })
            .path(dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint(format!(
                "{}/rocket-controls/src/meters/fuel-gauge",
                to_fish_style(1, tmp_dir.path().join("above-repo").to_slash_lossy(), "")
            ))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn directory_in_git_repo_truncate_to_repo_true() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("above-repo").join("rocket-controls");
        let dir = repo_dir.join("src/meters/fuel-gauge");
        fs::create_dir_all(&dir)?;
        init_repo(&repo_dir).unwrap();

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                // `truncate_to_repo = true` should display the truncated path
                truncation_length = 5
                truncate_to_repo = true
            })
            .path(dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan
                .bold()
                .paint("rocket-controls/src/meters/fuel-gauge")
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn symlinked_git_repo_root() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("rocket-controls");
        let symlink_dir = tmp_dir.path().join("rocket-controls-symlink");
        fs::create_dir(&repo_dir)?;
        init_repo(&repo_dir).unwrap();
        symlink(&repo_dir, &symlink_dir)?;

        let actual = ModuleRenderer::new("directory").path(symlink_dir).collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("rocket-controls-symlink")
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn directory_in_symlinked_git_repo() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("rocket-controls");
        let src_dir = repo_dir.join("src");
        let symlink_dir = tmp_dir.path().join("rocket-controls-symlink");
        let symlink_src_dir = symlink_dir.join("src");
        fs::create_dir_all(&src_dir)?;
        init_repo(&repo_dir).unwrap();
        symlink(&repo_dir, &symlink_dir)?;

        let actual = ModuleRenderer::new("directory")
            .path(symlink_src_dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("rocket-controls-symlink/src")
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn truncated_directory_in_symlinked_git_repo() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("rocket-controls");
        let src_dir = repo_dir.join("src/meters/fuel-gauge");
        let symlink_dir = tmp_dir.path().join("rocket-controls-symlink");
        let symlink_src_dir = symlink_dir.join("src/meters/fuel-gauge");
        fs::create_dir_all(&src_dir)?;
        init_repo(&repo_dir).unwrap();
        symlink(&repo_dir, &symlink_dir)?;

        let actual = ModuleRenderer::new("directory")
            .path(symlink_src_dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("src/meters/fuel-gauge")
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn directory_in_symlinked_git_repo_truncate_to_repo_false() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("above-repo").join("rocket-controls");
        let src_dir = repo_dir.join("src/meters/fuel-gauge");
        let symlink_dir = tmp_dir
            .path()
            .join("above-repo")
            .join("rocket-controls-symlink");
        let symlink_src_dir = symlink_dir.join("src/meters/fuel-gauge");
        fs::create_dir_all(&src_dir)?;
        init_repo(&repo_dir).unwrap();
        symlink(&repo_dir, &symlink_dir)?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                // Don't truncate the path at all.
                truncation_length = 5
                truncate_to_repo = false
            })
            .path(symlink_src_dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan
                .bold()
                .paint("above-repo/rocket-controls-symlink/src/meters/fuel-gauge")
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn fish_path_directory_in_symlinked_git_repo_truncate_to_repo_false() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("above-repo").join("rocket-controls");
        let src_dir = repo_dir.join("src/meters/fuel-gauge");
        let symlink_dir = tmp_dir
            .path()
            .join("above-repo")
            .join("rocket-controls-symlink");
        let symlink_src_dir = symlink_dir.join("src/meters/fuel-gauge");
        fs::create_dir_all(&src_dir)?;
        init_repo(&repo_dir).unwrap();
        symlink(&repo_dir, &symlink_dir)?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                // Don't truncate the path at all.
                truncation_length = 5
                truncate_to_repo = false
                fish_style_pwd_dir_length = 1
            })
            .path(symlink_src_dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint(format!(
                "{}/above-repo/rocket-controls-symlink/src/meters/fuel-gauge",
                to_fish_style(1, tmp_dir.path().to_slash_lossy(), "")
            ))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn fish_path_directory_in_symlinked_git_repo_truncate_to_repo_true() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("above-repo").join("rocket-controls");
        let src_dir = repo_dir.join("src/meters/fuel-gauge");
        let symlink_dir = tmp_dir
            .path()
            .join("above-repo")
            .join("rocket-controls-symlink");
        let symlink_src_dir = symlink_dir.join("src/meters/fuel-gauge");
        fs::create_dir_all(&src_dir)?;
        init_repo(&repo_dir).unwrap();
        symlink(&repo_dir, &symlink_dir)?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                // `truncate_to_repo = true` should display the truncated path
                truncation_length = 5
                truncate_to_repo = true
                fish_style_pwd_dir_length = 1
            })
            .path(symlink_src_dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint(format!(
                "{}/rocket-controls-symlink/src/meters/fuel-gauge",
                to_fish_style(1, tmp_dir.path().join("above-repo").to_slash_lossy(), "")
            ))
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn directory_in_symlinked_git_repo_truncate_to_repo_true() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("above-repo").join("rocket-controls");
        let src_dir = repo_dir.join("src/meters/fuel-gauge");
        let symlink_dir = tmp_dir
            .path()
            .join("above-repo")
            .join("rocket-controls-symlink");
        let symlink_src_dir = symlink_dir.join("src/meters/fuel-gauge");
        fs::create_dir_all(&src_dir)?;
        init_repo(&repo_dir).unwrap();
        symlink(&repo_dir, &symlink_dir)?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                // `truncate_to_repo = true` should display the truncated path
                truncation_length = 5
                truncate_to_repo = true
            })
            .path(symlink_src_dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan
                .bold()
                .paint("rocket-controls-symlink/src/meters/fuel-gauge")
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[ignore]
    fn symlinked_directory_in_git_repo() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("rocket-controls");
        let dir = repo_dir.join("src");
        fs::create_dir_all(&dir)?;
        init_repo(&repo_dir).unwrap();
        symlink(&dir, repo_dir.join("src/loop"))?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                // `truncate_to_repo = true` should display the truncated path
                truncation_length = 5
                truncate_to_repo = true
            })
            .path(repo_dir.join("src/loop/loop"))
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("rocket-controls/src/loop/loop")
        ));

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn truncation_symbol_truncated_root() -> io::Result<()> {
        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 3
                truncation_symbol = "…/"
            })
            .path(Path::new("/a/four/element/path"))
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("…/four/element/path")
        ));
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn truncation_symbol_not_truncated_root() -> io::Result<()> {
        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 4
                truncation_symbol = "…/"
            })
            .path(Path::new("/a/four/element/path"))
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("/a/four/element/path")
        ));
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn truncation_symbol_truncated_home() -> io::Result<()> {
        let (tmp_dir, name) = make_known_tempdir(home_dir().unwrap().as_path())?;
        let dir = tmp_dir.path().join("a/subpath");
        fs::create_dir_all(&dir)?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 3
                truncation_symbol = "…/"
            })
            .path(dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint(format!("…/{}/a/subpath", name))
        ));
        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn truncation_symbol_not_truncated_home() -> io::Result<()> {
        let (tmp_dir, name) = make_known_tempdir(home_dir().unwrap().as_path())?;
        let dir = tmp_dir.path().join("a/subpath");
        fs::create_dir_all(&dir)?;

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncate_to_repo = false // Necessary if homedir is a git repo
                truncation_length = 4
                truncation_symbol = "…/"
            })
            .path(dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint(format!("~/{}/a/subpath", name))
        ));
        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn truncation_symbol_truncated_in_repo() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("above").join("repo");
        let dir = repo_dir.join("src/sub/path");
        fs::create_dir_all(&dir)?;
        init_repo(&repo_dir).unwrap();

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 3
                truncation_symbol = "…/"
            })
            .path(dir)
            .collect();
        let expected = Some(format!("{} ", Color::Cyan.bold().paint("…/src/sub/path")));
        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn truncation_symbol_not_truncated_in_repo() -> io::Result<()> {
        let (tmp_dir, _) = make_known_tempdir(Path::new("/tmp"))?;
        let repo_dir = tmp_dir.path().join("above").join("repo");
        let dir = repo_dir.join("src/sub/path");
        fs::create_dir_all(&dir)?;
        init_repo(&repo_dir).unwrap();

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 5
                truncation_symbol = "…/"
                truncate_to_repo = true
            })
            .path(dir)
            .collect();
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("…/repo/src/sub/path")
        ));
        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[cfg(target_os = "windows")]
    fn truncation_symbol_windows_root_not_truncated() -> io::Result<()> {
        let dir = Path::new("C:\\temp");
        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 2
                truncation_symbol = "…/"
            })
            .path(dir)
            .collect();
        let expected = Some(format!("{} ", Color::Cyan.bold().paint("C:/temp")));
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    #[cfg(target_os = "windows")]
    fn truncation_symbol_windows_root_truncated() -> io::Result<()> {
        let dir = Path::new("C:\\temp");
        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 1
                truncation_symbol = "…/"
            })
            .path(dir)
            .collect();
        let expected = Some(format!("{} ", Color::Cyan.bold().paint("…/temp")));
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    #[cfg(target_os = "windows")]
    fn truncation_symbol_windows_root_truncated_backslash() -> io::Result<()> {
        let dir = Path::new("C:\\temp");
        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                truncation_length = 1
                truncation_symbol = r"…\"
            })
            .path(dir)
            .collect();
        let expected = Some(format!("{} ", Color::Cyan.bold().paint("…\\temp")));
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn use_logical_path_true_should_render_logical_dir_path() -> io::Result<()> {
        let tmp_dir = TempDir::new()?;
        let path = tmp_dir.path().join("src/meters/fuel-gauge");
        fs::create_dir_all(&path)?;
        let logical_path = "Logical:/fuel-gauge";

        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("Logical:/fuel-gauge")
        ));

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                use_logical_path = true
                truncation_length = 3
            })
            .path(path)
            .logical_path(logical_path)
            .collect();

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    fn use_logical_path_false_should_render_current_dir_path() -> io::Result<()> {
        let tmp_dir = TempDir::new()?;
        let path = tmp_dir.path().join("src/meters/fuel-gauge");
        fs::create_dir_all(&path)?;
        let logical_path = "Logical:/fuel-gauge";

        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("src/meters/fuel-gauge")
        ));

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                use_logical_path = false
                truncation_length = 3
            })
            .path(path)
            .logical_path(logical_path) // logical_path should be ignored
            .collect();

        assert_eq!(expected, actual);
        tmp_dir.close()
    }

    #[test]
    #[cfg(windows)]
    fn windows_trims_extended_path_prefix() {
        // Under Windows, path canonicalization returns the paths using extended-path prefixes `\\?\`
        // We expect this prefix to be trimmed before being rendered.
        let sys32_path = Path::new(r"\\?\C:\Windows\System32");

        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint("C:/Windows/System32")
        ));

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                use_logical_path = false
                truncation_length = 0
            })
            .path(sys32_path)
            .collect();

        assert_eq!(expected, actual);
    }

    #[test]
    #[cfg(windows)]
    fn windows_trims_extended_unc_path_prefix() {
        // Under Windows, path canonicalization returns UNC paths using extended-path prefixes `\\?\UNC\`
        // We expect this prefix to be trimmed before being rendered.
        let unc_path = Path::new(r"\\?\UNC\server\share\a\b\c");

        // NOTE: path-slash doesn't convert slashes which are part of path prefixes under Windows,
        // which is why the first part of this string still includes backslashes
        let expected = Some(format!(
            "{} ",
            Color::Cyan.bold().paint(r"\\server\share/a/b/c")
        ));

        let actual = ModuleRenderer::new("directory")
            .config(toml::toml! {
                [directory]
                use_logical_path = false
                truncation_length = 0
            })
            .path(unc_path)
            .collect();

        assert_eq!(expected, actual);
    }
}
