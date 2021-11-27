node {
    checkout scm
    def customImage = docker.build("leiningen-image:${env.BUILD_ID}")

    customImage.inside('-v $HOME/.m2:/home/jenkins/.m2') {
        stage('Test') {
            sh 'lein cloverage --codecov'
        }
    }
}
