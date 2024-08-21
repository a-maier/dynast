use std::sync::LazyLock;

pub const NAME: &str = env!("CARGO_PKG_NAME");
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub const GIT_REV: Option<&str> = option_env!("VERGEN_GIT_SHA");
pub const GIT_BRANCH: Option<&str> = option_env!("VERGEN_GIT_BRANCH");

pub static VERSION_MAJOR: LazyLock<u32> = LazyLock::new(||
   env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap()
);

pub static VERSION_MINOR: LazyLock<u32> = LazyLock::new(||
   env!("CARGO_PKG_VERSION_MINOR").parse().unwrap()
);

pub static VERSION_PATCH: LazyLock<u32> = LazyLock::new(||
   env!("CARGO_PKG_VERSION_PATCH").parse().unwrap()
);

pub static VERSION_STRING: LazyLock<String> = LazyLock::new(||
   if let (Some(rev), Some(branch)) = (GIT_REV, GIT_BRANCH) {
       format!("{NAME} {VERSION} rev {rev} ({branch})")
   } else {
       format!("{NAME} {VERSION}")
   }
);
