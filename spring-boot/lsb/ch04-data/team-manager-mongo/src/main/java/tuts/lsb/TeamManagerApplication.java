package tuts.lsb;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import javax.annotation.PostConstruct;

/**
 * Run Development : SPRING_PROFILES_ACTIVE=production ./gradlew bootRun
 *
 * Run Production : SPRING_PROFILES_ACTIVE=production ./gradlew bootRun
 *
 * Spring Data REST POST:
 *
 * |*| : Create a new 'teammate (15)' (with a POST)
 * curl -i -X POST -H 'Content-Type:application/json' -d
 *   '{"firstName":"Wibble","lastName":"Wobble", "position":"skyguard"}'
 *   localhost:8080/teammates
 *
 * |*| : Assign 'teammate 15' to 'team 1' (with a PUT) (change relationship property)
 * curl -i http://localhost:8080/teammates/15/team
 *
 * |*| : Retrieve the new 'teammate 15' (with a PUT) (change normal property)
 * curl -i -X PUT -H 'Content-Type:text/uri-list'
 *   -d http://localhost:8080/teams/1
 *   localhost:8080/teammates/15/team
 *
 * |*| : Change 'position' of 'teammate 15'. (with a PATCH)
 * curl -i -X PATCH -H "Content-Type:application/json"
 *   -d '{"position":"short stop"}'
 *   localhost:8080/teammates/15
 *
 * |*| : Remove 'teammate 15'. (with a DELETE)
 * curl -i -X DELETE localhost:8080/teammates/2
 *
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
