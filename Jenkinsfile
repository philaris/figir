#!/usr/bin/env groovy

pipeline {
    agent any

    stages {
        stage('BuildCheck') {
            steps {
                echo 'Building and checking ...'
                sh "Rscript -e 'devtools::check()'"
            }
        }
        stage('Coverage') {
            steps {
                echo 'Calculating test coverage ...'
                sh "Rscript -e 'print(devtools::test_coverage(show_report = FALSE))'"
            }
        }
    }
    post {
        cleanup {
            cleanWs()
        }
    }
}
