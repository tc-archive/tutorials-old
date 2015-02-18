package tuts.lsb;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * Created by Temple on 15/02/15.
 */
@Service
//@Profile("!production")
public class DatabaseLoader {

    private final TeammateRepository teammateRepository;
    private final TeamRepository teamRepository;

    @Autowired
    public DatabaseLoader(
            TeammateRepository teammateRepository,
            TeamRepository teamRepository
            ) {
        this.teammateRepository = teammateRepository;
        this.teamRepository = teamRepository;
    }

    // @PostConstruct
    private void initDatabase() {

        Team springBootTeam = new Team("Spring Boot Hamsters");
        teamRepository.save(springBootTeam);

        Teammate greg = new Teammate("Temple", "Cloud");
        greg.setPosition("2nd base");
        greg.setTeam(springBootTeam);
        teammateRepository.save(greg);

        Teammate roy = new Teammate("Noodle", "Pancakes");
        roy.setPosition("1st base");
        roy.setTeam(springBootTeam);
        teammateRepository.save(roy);

        Teammate phil = new Teammate("Hank", "Marvin");
        phil.setPosition("pitcher");
        phil.setTeam(springBootTeam);
        teammateRepository.save(phil);
    }

}
