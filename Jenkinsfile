node {
    docker.image('clojure:openjdk-11-lein-2.9.8-slim-bullseye').inside {
        stage('Test') {
            sh 'lein cloverage --codecov'
        }
    }
}
