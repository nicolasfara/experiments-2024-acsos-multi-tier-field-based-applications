plugins {
    id("com.gradle.develocity") version "3.18.2"
    id("org.gradle.toolchains.foojay-resolver-convention") version "0.8.0"
}

develocity {
    buildScan {
        termsOfUseUrl = "https://gradle.com/terms-of-service"
        termsOfUseAgree = "yes"
        uploadInBackground = !System.getenv("CI").toBoolean()
    }
}

rootProject.name = "experiments-2024-acsos-multi-tier-field-based-applications"
