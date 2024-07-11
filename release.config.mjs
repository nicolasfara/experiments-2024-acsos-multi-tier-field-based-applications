const prepareCmd = `
echo version=\${nextRelease.version} > gradle.properties
echo VERSION="\${nextRelease.version}" > .env
echo PROJECT_NAME=$(grep -Po 'rootProject\\s*\\.\\s*name\\s*=\\s*"\\K[\\w-]+(?=")' settings.gradle.kts) >> .env
docker compose build
`
const publishCmd = `
docker compose push
git add gradle.properties .env
git commit -m "chore(release): update gradle.properties .env versions to \${nextRelease.version} [skip ci]"
git push
`
import config from 'semantic-release-preconfigured-conventional-commits' assert { type: "json" };

config.plugins.push(
    ["@semantic-release/exec", {
        "prepareCmd": prepareCmd,
        "publishCmd": publishCmd,
    }],
    ["@semantic-release/github", {
        "assets": [ 
            { "path": "charts.tar.zst" },
         ]
    }],
    "@semantic-release/git",
)

export default config
