package tuts.lsb;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import javax.annotation.PostConstruct;

/**
 * SPRING_PROFILES_ACTIVE=production ./gradlew bootRun
 */
@SpringBootApplication
// @DependsOn("databaseLoader")
public class TeamManagerApplication {

    private static final Logger LOG =
            LoggerFactory.getLogger(TeamManagerApplication.class);

    @Autowired
    TeammateRepository teammateRepository;

    @Autowired(required = false)
    DatabaseLoader databaseLoader;

    @PostConstruct
    void seeTheRoster() {
        for (Teammate teammate : teammateRepository.findAll()) {
            LOG.info(teammate.toString());
        }
    }

    public static void main(String[] args) {
        SpringApplication.run(TeamManagerApplication.class, args);
    }

}
