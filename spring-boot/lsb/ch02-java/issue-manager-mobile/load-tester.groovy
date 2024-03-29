package learningspringboot

import groovy.util.logging.Slf4j
@Grab('org.codehaus.gpars:gpars:1.1.0')
import groovyx.gpars.GParsPool
@Grab('org.codehaus.gpars:gpars:1.1.0')
import groovyx.gpars.GParsPool
import org.springframework.boot.CommandLineRunner

@Slf4j
class LoadTester implements CommandLineRunner {
    void run(String[] args) {
        GParsPool.withPool(8) {
            def loadset = ["http://localhost:8080"]*100
            loadset.eachParallel { url ->
                def results = url.toURL().text
                log.info("Hit ${url}")
            }
        }
    }
}