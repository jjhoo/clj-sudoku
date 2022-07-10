node {
    checkout scm
    def userId = sh(script: "id -u ${USER}", returnStdout: true).trim()
    def customImage = docker.build("build-clj-sudoku:${env.BUILD_ID}", "--build-arg JENKINS_UID=${userId} -f .jenkins/docker/Dockerfile .jenkins/docker")

    sh 'mkdir -p ${WORKSPACE_TMP}/m2'

    withCredentials([string(credentialsId: 'coverage-token', variable: 'COVERAGE_TOKEN')]) {
        cache(maxCacheSize: 250, defaultBranch: 'master', caches: [
            [$class: 'ArbitraryFileCache', path: "${env.WORKSPACE_TMP}/m2", cacheValidityDecidingFile: 'project.clj', compressionMethod: 'TARGZ']
            customImage.inside("-v ${env.WORKSPACE_TMP}/m2:/home/jenkins/.m2") {
                stage('Deps') {
                   sh 'lein deps'
                }
                stage('Test') {
                   sh 'lein test'
                }
                stage('Coverage') {
                   sh 'lein cloverage --codecov'
                   sh './scripts/codecov.sh -t $COVERAGE_TOKEN -K'
                }
            }
        }
    }
}
