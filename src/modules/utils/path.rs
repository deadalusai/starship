use std::borrow::Cow;
use std::path::{Component, Path, Prefix};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PathKind {
    /// No prefix/relative path fragment, e.g. `cat_pics` or `\\?\cat_pics`
    Relative,
    /// Relative path with Windows disk/drive prefix e.g. `C:` or `\\?\C:`
    ///
    /// TODO: Need to confirm we need to handle this case one.
    /// RelativeDrive paths can only really be interpreted when changing directories,
    /// E.g.
    ///     > cd D:foo/bar
    /// Changes to the D drive, then changes to the foo/bar directory relative to the PWD on that drive.
    /// Having changed directories, your actual PWD would be (something like) "D:/root/foo/bar"
    RelativeDrive(char),
    /// Absolute path e.g. `/foo` or `\foo`
    Absolute,
    /// Absolute path with Windows disk/drive prefix e.g. `C:\` or `\\?\C:\`
    AbsoluteDrive(char),
    /// Windows' UNC path, e.g. `\\server\share` or `\\?\UNC\server\share`
    AbsoluteUnc,
    /// Windows device path. e.g. \\.\COM42
    AbsoluteDevice,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NormalizedPath<'a> {
    pub kind: PathKind,
    pub segments: Vec<Cow<'a, str>>,
}

impl<'a> NormalizedPath<'a> {
    #[cfg(test)]
    pub fn new<I>(kind: PathKind, segments: I) -> NormalizedPath<'a>
    where
        I: IntoIterator<Item = &'a str>,
    {
        NormalizedPath {
            kind,
            segments: segments.into_iter().map(|s| Cow::Borrowed(s)).collect(),
        }
    }

    pub fn is_absolute(&self) -> bool {
        match self.kind {
            PathKind::Relative | PathKind::RelativeDrive(_) => false,
            PathKind::Absolute
            | PathKind::AbsoluteDrive(_)
            | PathKind::AbsoluteUnc
            | PathKind::AbsoluteDevice => true,
        }
    }

    pub fn starts_with(&self, other: &NormalizedPath) -> bool {
        self.kind == other.kind && self.segments.starts_with(&other.segments)
    }

    pub fn replace_sub_path(
        &mut self,
        sub_path: &NormalizedPath,
        replacement_path: &NormalizedPath,
    ) -> bool {
        // Try and replace from the start of the path
        if self.starts_with(sub_path) {
            let needle = ..sub_path.segments.len();
            let replacement = replacement_path
                .segments
                .iter()
                .map(|s| Cow::Owned(s.to_string()));
            self.segments.splice(needle, replacement);
            self.kind = replacement_path.kind;
            return true;
        }
        if !replacement_path.is_absolute() {
            // Can't insert an absolute path to into the middle of another path.
            return false;
        }
        // Search for the sub-sequence to replace
        if let Some(needle) = find_needle(&self.segments, &sub_path.segments) {
            let replacement = replacement_path
                .segments
                .iter()
                .map(|s| Cow::Owned(s.to_string()));
            self.segments.splice(needle, replacement);
            return true;
        }
        // Nothing replaced
        false
    }
}

fn find_needle<T: PartialEq>(haystack: &[T], needle: &[T]) -> Option<std::ops::Range<usize>> {
    // Invalid needle?
    if needle.is_empty() {
        return None;
    }
    // Scan the haystack for the needle
    haystack
        .windows(needle.len())
        .position(|window| window == needle)
        .map(|start| start..(start + needle.len()))
}

pub trait PathExt {
    /// Normalizes a Path into a manipulatable and separator-agnostic structure
    fn normalize(&self) -> NormalizedPath;
}

impl PathExt for Path {
    fn normalize(&self) -> NormalizedPath {
        let mut components = self.components();
        let mut segments = Vec::new();
        // Determine the path kind by examining the start of the path.
        let kind = match components.next() {
            Some(Component::Prefix(prefix)) => {
                // Windows path handling:
                // Normalise Verbatim and Non-Verbatim path prefixes into a comparable structure.
                // NOTE: "Verbatim" paths are the rust std library's name for Windows extended-path prefixed paths.
                match prefix.kind() {
                    Prefix::Verbatim(segment) => {
                        segments.push(segment.to_string_lossy());
                        PathKind::Relative
                    }
                    Prefix::DeviceNS(name) => {
                        segments.push(name.to_string_lossy());
                        PathKind::AbsoluteDevice
                    }
                    Prefix::VerbatimUNC(server, share) | Prefix::UNC(server, share) => {
                        segments.push(server.to_string_lossy());
                        segments.push(share.to_string_lossy());
                        PathKind::AbsoluteUnc
                    }
                    Prefix::VerbatimDisk(letter) | Prefix::Disk(letter) => {
                        // Scan the next component to determine if this
                        // is a relative or absolute drive path
                        match components.next() {
                            Some(Component::Prefix(_)) => unreachable!(),
                            Some(Component::RootDir) => {
                                // E.g. C:/absolute/path
                                PathKind::AbsoluteDrive(letter as char)
                            }
                            Some(c) => {
                                // E.g. C:relative/path
                                segments.push(c.as_os_str().to_string_lossy());
                                PathKind::RelativeDrive(letter as char)
                            }
                            None => {
                                // E.g. C:
                                PathKind::RelativeDrive(letter as char)
                            }
                        }
                    }
                }
            }
            Some(Component::RootDir) => {
                // Unix absolute paths and Windows paths without a prefix
                PathKind::Absolute
            }
            Some(c) => {
                // Relative paths
                segments.push(c.as_os_str().to_string_lossy());
                PathKind::Relative
            }
            None => {
                // Empty path
                PathKind::Relative
            }
        };
        for c in components {
            // Skip any "RootDir" segments which may follow the "Prefix" consumed above,
            // we treat everything after the prefix as normal path segments.
            if c == Component::RootDir {
                continue;
            }
            segments.push(c.as_os_str().to_string_lossy());
        }
        NormalizedPath { kind, segments }
    }
}

#[cfg(test)]
#[cfg(windows)]
mod windows {
    use super::*;

    #[test]
    fn normalize_path() {
        assert_eq!(
            Path::new(r"").normalize(),
            NormalizedPath::new(PathKind::Relative, Vec::<&str>::new())
        );
        assert_eq!(
            Path::new(r"a/b/c/d").normalize(),
            NormalizedPath::new(PathKind::Relative, vec!["a", "b", "c", "d"])
        );
        assert_eq!(
            Path::new(r"/a/b/c/d").normalize(),
            NormalizedPath::new(PathKind::Absolute, vec!["a", "b", "c", "d"])
        );
        assert_eq!(
            Path::new(r"C:").normalize(),
            NormalizedPath::new(PathKind::RelativeDrive('C'), Vec::<&str>::new())
        );
        assert_eq!(
            Path::new(r"C:\").normalize(),
            NormalizedPath::new(PathKind::AbsoluteDrive('C'), Vec::<&str>::new())
        );
        assert_eq!(
            Path::new(r"C:a\b\c\d").normalize(),
            NormalizedPath::new(PathKind::RelativeDrive('C'), vec!["a", "b", "c", "d"])
        );
        assert_eq!(
            Path::new(r"C:\a\b\c\d").normalize(),
            NormalizedPath::new(PathKind::AbsoluteDrive('C'), vec!["a", "b", "c", "d"])
        );
        assert_eq!(
            Path::new(r"\\Se\Sh\a\b\c\d").normalize(),
            NormalizedPath::new(PathKind::AbsoluteUnc, vec!["Se", "Sh", "a", "b", "c", "d"])
        );
        assert_eq!(
            Path::new(r"\\?\C:\a\b\c\d").normalize(),
            NormalizedPath::new(PathKind::AbsoluteDrive('C'), vec!["a", "b", "c", "d"])
        );
        assert_eq!(
            Path::new(r"\\?\UNC\Se\Sh\a\b\c\d").normalize(),
            NormalizedPath::new(PathKind::AbsoluteUnc, vec!["Se", "Sh", "a", "b", "c", "d"])
        );
        assert_eq!(
            Path::new(r"\\.\COM42\a\b\c\d").normalize(),
            NormalizedPath::new(PathKind::AbsoluteDevice, vec!["COM42", "a", "b", "c", "d"])
        );
    }

    #[test]
    fn normalized_equals() {
        fn test_equals(a: &Path, b: &Path) {
            let a = a.normalize();
            let b = b.normalize();
            assert!(a == b);
            assert!(b == a);
        }

        // UNC paths
        let verbatim_unc = Path::new(r"\\?\UNC\server\share\sub\path");
        let unc = Path::new(r"\\server\share\sub\path");
        test_equals(&verbatim_unc, &verbatim_unc);
        test_equals(&verbatim_unc, &unc);
        test_equals(&unc, &unc);
        test_equals(&unc, &verbatim_unc);

        // Disk paths
        let verbatim_disk = Path::new(r"\\?\C:\test\path");
        let disk = Path::new(r"C:\test\path");
        test_equals(&verbatim_disk, &verbatim_disk);
        test_equals(&verbatim_disk, &disk);
        test_equals(&disk, &disk);
        test_equals(&disk, &verbatim_disk);

        // Other paths
        let verbatim = Path::new(r"\\?\cat_pics");
        let no_prefix = Path::new(r"\cat_pics");
        let device_ns = Path::new(r"\\.\COM42");
        test_equals(&verbatim, &verbatim);
        test_equals(&no_prefix, &no_prefix);
        test_equals(&device_ns, &device_ns);
    }

    #[test]
    fn normalized_equals_differing_prefixes() {
        fn test_not_equals(a: &Path, b: &Path) {
            let a = a.normalize();
            let b = b.normalize();
            assert!(a != b);
            assert!(b != a);
        }

        let verbatim_unc = Path::new(r"\\?\UNC\server\share\sub\path");
        let unc = Path::new(r"\\server\share\sub\path");
        let verbatim_disk = Path::new(r"\\?\C:\test\path");
        let disk = Path::new(r"C:\test\path");
        let verbatim = Path::new(r"\\?\cat_pics");
        let no_prefix = Path::new(r"\cat_pics");
        let device_ns = Path::new(r"\\.\COM42");

        test_not_equals(&verbatim_unc, &verbatim_disk);
        test_not_equals(&unc, &disk);
        test_not_equals(&disk, &device_ns);
        test_not_equals(&device_ns, &verbatim_disk);
        test_not_equals(&no_prefix, &unc);
        test_not_equals(&no_prefix, &verbatim);
    }

    #[test]
    fn normalized_starts_with() {
        fn test_starts_with(a: &Path, b: &Path) {
            let a = a.normalize();
            let b = b.normalize();
            assert!(a.starts_with(&b));
            assert!(!b.starts_with(&a));
        }

        // UNC paths
        let verbatim_unc_a = Path::new(r"\\?\UNC\server\share\a\b\c\d");
        let verbatim_unc_b = Path::new(r"\\?\UNC\server\share\a\b");
        let unc_a = Path::new(r"\\server\share\a\b\c\d");
        let unc_b = Path::new(r"\\server\share\a\b");
        test_starts_with(&verbatim_unc_a, &verbatim_unc_b);
        test_starts_with(&unc_a, &unc_b);
        test_starts_with(&verbatim_unc_a, &unc_b);
        test_starts_with(&unc_a, &verbatim_unc_b);

        // Disk paths
        let verbatim_disk_a = Path::new(r"\\?\C:\a\b\c\d");
        let verbatim_disk_b = Path::new(r"\\?\C:\a\b");
        let disk_a = Path::new(r"C:\a\b\c\d");
        let disk_b = Path::new(r"C:\a\b");
        test_starts_with(&verbatim_disk_a, &verbatim_disk_b);
        test_starts_with(&disk_a, &disk_b);
        test_starts_with(&disk_a, &verbatim_disk_b);
        test_starts_with(&verbatim_disk_a, &disk_b);

        // Other paths
        let verbatim_a = Path::new(r"\\?\cat_pics\a\b\c\d");
        let verbatim_b = Path::new(r"\\?\cat_pics\a\b");
        let device_ns_a = Path::new(r"\\.\COM43\a\b\c\d");
        let device_ns_b = Path::new(r"\\.\COM43\a\b");
        let no_prefix_a = Path::new(r"\a\b\c\d");
        let no_prefix_b = Path::new(r"\a\b");
        test_starts_with(&verbatim_a, &verbatim_b);
        test_starts_with(&device_ns_a, &device_ns_b);
        test_starts_with(&no_prefix_a, &no_prefix_b);
    }

    #[test]
    fn normalized_starts_with_differing_prefixes() {
        fn test_not_starts_with(a: &Path, b: &Path) {
            assert!(!a.starts_with(&b));
            assert!(!b.starts_with(&a));
        }

        let verbatim_unc = Path::new(r"\\?\UNC\server\share\a\b\c\d");
        let unc = Path::new(r"\\server\share\a\b\c\d");
        let verbatim_disk = Path::new(r"\\?\C:\a\b\c\d");
        let disk = Path::new(r"C:\a\b\c\d");
        let verbatim = Path::new(r"\\?\cat_pics\a\b\c\d");
        let device_ns = Path::new(r"\\.\COM43\a\b\c\d");
        let no_prefix = Path::new(r"\a\b\c\d");

        test_not_starts_with(&verbatim_unc, &device_ns);
        test_not_starts_with(&unc, &device_ns);
        test_not_starts_with(&verbatim_disk, &verbatim);
        test_not_starts_with(&disk, &verbatim);
        test_not_starts_with(&disk, &unc);
        test_not_starts_with(&verbatim_disk, &no_prefix);
    }

    #[test]
    fn replace_sub_path() {
        let abs_path = Path::new(r"C:\a\b\c\d").normalize();
        let abs_sub_path = Path::new(r"C:\a\b").normalize();
        let abs_replacement = Path::new(r"D:\x\y\z").normalize();

        let rel_path = Path::new(r"a\b\c\d").normalize();
        let rel_sub_path = Path::new(r"b\c").normalize();
        let rel_replacement = Path::new(r"x\y\z").normalize();

        // abs path, abs sub path, abs replacement
        let mut path = abs_path.clone();
        assert_eq!(
            path.replace_sub_path(&abs_sub_path, &abs_replacement),
            true
        );
        assert_eq!(path.kind, abs_replacement.kind); // kind replaced
        assert_eq!(&path.segments, &["x", "y", "z", "c", "d"]);

        // abs path, rel sub path, abs replacement
        let mut path = abs_path.clone();
        assert_eq!(
            path.replace_sub_path(&rel_sub_path, &abs_replacement),
            false
        );
        assert_eq!(path, abs_path); // unchanged

        // abs path, rel sub path, rel replacement
        let mut path = abs_path.clone();
        assert_eq!(
            path.replace_sub_path(&rel_sub_path, &rel_replacement),
            true
        );
        assert_eq!(path.kind, abs_path.kind); // kind unchanged
        assert_eq!(&path.segments, &["a", "x", "y", "z", "d"]);

        // rel path, abs sub path, abs replacement
        let mut path = rel_path.clone();
        assert_eq!(
            path.replace_sub_path(&abs_sub_path, &abs_replacement),
            false
        );
        assert_eq!(path, rel_path); // unchanged (unable to match abs sub path)

        // rel path, rel sub path, abs replacement
        let mut path = rel_path.clone();
        assert_eq!(
            path.replace_sub_path(&rel_sub_path, &abs_replacement),
            false
        );
        assert_eq!(path, rel_path); // unchanged (unable to replace rel sub path with abs path)

        // rel path, rel sub path, rel replacement
        let mut path = rel_path.clone();
        assert_eq!(
            path.replace_sub_path(&rel_sub_path, &rel_replacement),
            true
        );
        assert_eq!(path.kind, rel_path.kind); // kind unchanged
        assert_eq!(&path.segments, &["a", "x", "y", "z", "d"]);
    }
}

#[cfg(test)]
#[cfg(not(windows))]
mod nix {
    use super::*;

    #[test]
    fn normalize_path() {
        assert_eq!(
            Path::new(r"").normalize(),
            NormalizedPath::new(PathKind::Relative, Vec::<&str>::new())
        );
        assert_eq!(
            Path::new(r"a/b/c/d").normalize(),
            NormalizedPath::new(PathKind::Relative, vec!["a", "b", "c", "d"])
        );
        assert_eq!(
            Path::new(r"/a/b/c/d").normalize(),
            NormalizedPath::new(PathKind::Absolute, vec!["a", "b", "c", "d"])
        );
        // Windows prefixes/path seperators are not parsed on Unix
        assert_eq!(
            Path::new(r"C:\a\b\c\d").normalize(),
            NormalizedPath::new(PathKind::Relative, vec![r"C:\a\b\c\d"])
        );
    }

    #[test]
    fn normalized_equals() {
        let path_a = Path::new("/a/b/c/d").normalize();
        let path_b = Path::new("/a/b/c/d").normalize();
        assert!(path_a == path_b);
        assert!(path_b == path_a);

        let path_c = Path::new("/a/b").normalize();
        assert!(path_a != path_c);
    }

    #[test]
    fn normalized_equals_differing_prefixes() {
        // Windows path prefixes are not parsed on *nix
        let path_a = Path::new(r"\\?\UNC\server\share\a\b\c\d").normalize();
        let path_b = Path::new(r"\\server\share\a\b\c\d").normalize();
        assert!(path_a != path_b);
        assert!(path_b != path_a);
        assert!(path_a == path_a);
    }

    #[test]
    fn normalized_starts_with() {
        let path_a = Path::new("/a/b/c/d").normalize();
        let path_b = Path::new("/a/b").normalize();
        assert!(path_a.starts_with(&path_b));
        assert!(!path_b.starts_with(&path_a));
    }

    #[test]
    fn normalized_starts_with_differing_prefixes() {
        // Windows path prefixes are not parsed on *nix
        let path_a = Path::new(r"\\?\UNC\server\share\a\b\c\d").normalize();
        let path_b = Path::new(r"\\server\share\a\b").normalize();
        assert!(!path_a.starts_with(&path_b));
        assert!(!path_b.starts_with(&path_a));
        assert!(path_a.starts_with(&path_a));
    }

    #[test]
    fn replace_sub_path() {
        let abs_path = Path::new(r"/a/b/c/d").normalize();
        let abs_sub_path = Path::new(r"/a/b").normalize();
        let abs_replacement = Path::new(r"/x/y/z").normalize();

        let rel_path = Path::new(r"a/b/c/d").normalize();
        let rel_sub_path = Path::new(r"b/c").normalize();
        let rel_replacement = Path::new(r"x/y/z").normalize();

        // abs path, abs sub path, abs replacement
        let mut path = abs_path.clone();
        assert_eq!(
            path.replace_sub_path(&abs_sub_path, &abs_replacement),
            true
        );
        assert_eq!(path.kind, abs_replacement.kind); // kind replaced
        assert_eq!(&path.segments, &["x", "y", "z", "c", "d"]);

        // abs path, rel sub path, abs replacement
        let mut path = abs_path.clone();
        assert_eq!(
            path.replace_sub_path(&rel_sub_path, &abs_replacement),
            false
        );
        assert_eq!(path, abs_path); // unchanged

        // abs path, rel sub path, rel replacement
        let mut path = abs_path.clone();
        assert_eq!(
            path.replace_sub_path(&rel_sub_path, &rel_replacement),
            true
        );
        assert_eq!(path.kind, abs_path.kind); // kind unchanged
        assert_eq!(&path.segments, &["a", "x", "y", "z", "d"]);

        // rel path, abs sub path, abs replacement
        let mut path = rel_path.clone();
        assert_eq!(
            path.replace_sub_path(&abs_sub_path, &abs_replacement),
            false
        );
        assert_eq!(path, rel_path); // unchanged (unable to match abs sub path)

        // rel path, rel sub path, abs replacement
        let mut path = rel_path.clone();
        assert_eq!(
            path.replace_sub_path(&rel_sub_path, &abs_replacement),
            false
        );
        assert_eq!(path, rel_path); // unchanged (unable to replace rel sub path with abs path)

        // rel path, rel sub path, rel replacement
        let mut path = rel_path.clone();
        assert_eq!(
            path.replace_sub_path(&rel_sub_path, &rel_replacement),
            true
        );
        assert_eq!(path.kind, rel_path.kind); // kind unchanged
        assert_eq!(&path.segments, &["a", "x", "y", "z", "d"]);
    }
}
