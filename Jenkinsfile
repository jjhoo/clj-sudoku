node {
    docker.image('clojure:openjdk-11-lein-2.9.8-slim-bullseye').inside('-u root:root -v $HOME/.lein:/root/.lein --entrypoint=') {
        stage('Test') {
            sh 'ls -la ~/'
            sh 'id'
            sh 'lein cloverage --codecov'
        }
    }
}
