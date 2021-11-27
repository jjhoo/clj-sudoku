node {
    checkout scm
    def customImage = docker.build("leiningen-image:${env.BUILD_ID}")

    withCredentials([string(credentialsId: 'COVERAGE_TOKEN', variable: 'COVERAGE_TOKEN')]) {
        customImage.inside('-v $HOME/.m2:/home/jenkins/.m2') {
            stage('Test') {
               sh 'lein cloverage --codecov'
               sh './scripts/codecov.sh -t $COVERAGE_TOKEN'
            }
        }
    }
}
