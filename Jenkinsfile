node {
    checkout scm
    def customImage = docker.build("build-clj-sudoku:${env.BUILD_ID}", "-f .jenkins/docker/Dockerfile .jenkins/docker")

    withCredentials([string(credentialsId: 'coverage-token', variable: 'COVERAGE_TOKEN')]) {
        customImage.inside('-v $HOME/.m2:/home/jenkins/.m2') {
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
