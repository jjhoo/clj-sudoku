node {
    checkout scm
    def customImage = docker.build("leiningen-image:${env.BUILD_ID}")

    customImage.inside('-v $HOME/.lein:/home/jenkins/.lein') {
        stage('Test') {
            sh 'ls -la ~/'
            sh 'id'
            sh 'lein cloverage --codecov'
        }
    }
}
