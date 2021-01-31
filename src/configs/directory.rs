use crate::config::{ModuleConfig, RootModuleConfig};
use indexmap::IndexMap;

use starship_module_config_derive::ModuleConfig;

#[derive(Clone, PartialEq, Eq)]
pub enum PathSeparatorOption {
    /// (Default) E.g. `/`
    Slash,
    /// E.g. `\`
    Backslash,
}

impl<'a> ModuleConfig<'a> for PathSeparatorOption {
    fn from_config(config: &toml::Value) -> Option<Self> {
        match config.as_str() {
            Some("slash") => Some(PathSeparatorOption::Slash),
            Some("backslash") => Some(PathSeparatorOption::Backslash),
            _ => None,
        }
    }
}

#[derive(Clone, ModuleConfig)]
pub struct DirectoryConfig<'a> {
    pub truncation_length: usize,
    pub truncate_to_repo: bool,
    pub substitutions: IndexMap<String, &'a str>,
    pub fish_style_pwd_dir_length: usize,
    pub use_logical_path: bool,
    pub path_separator: PathSeparatorOption,
    pub format: &'a str,
    pub style: &'a str,
    pub disabled: bool,
    pub read_only: &'a str,
    pub read_only_style: &'a str,
    pub truncation_symbol: &'a str,
    pub home_symbol: &'a str,
}

impl<'a> RootModuleConfig<'a> for DirectoryConfig<'a> {
    fn new() -> Self {
        DirectoryConfig {
            truncation_length: 3,
            truncate_to_repo: true,
            fish_style_pwd_dir_length: 0,
            use_logical_path: true,
            path_separator: PathSeparatorOption::Slash,
            substitutions: IndexMap::new(),
            format: "[$path]($style)[$read_only]($read_only_style) ",
            style: "cyan bold",
            disabled: false,
            read_only: "🔒",
            read_only_style: "red",
            truncation_symbol: "",
            home_symbol: "~",
        }
    }
}
