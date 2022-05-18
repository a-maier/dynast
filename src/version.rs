use lazy_static::lazy_static;

pub const NAME: &str = env!("CARGO_PKG_NAME");
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub const GIT_REV: Option<&str> = option_env!("VERGEN_GIT_SHA_SHORT");
pub const GIT_BRANCH: Option<&str> = option_env!("VERGEN_GIT_BRANCH");

lazy_static! {
    pub static ref VERSION_MAJOR: u32 =
        env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap();
    pub static ref VERSION_MINOR: u32 =
        env!("CARGO_PKG_VERSION_MINOR").parse().unwrap();
    pub static ref VERSION_PATCH: u32 =
        env!("CARGO_PKG_VERSION_PATCH").parse().unwrap();

    pub static ref VERSION_STRING: String = {
        if let (Some(rev), Some(branch)) = (GIT_REV, GIT_BRANCH) {
            format!("{NAME} {VERSION} rev {rev} ({branch})")
        } else {
            format!("{NAME} {VERSION}")
        }
    };
}
