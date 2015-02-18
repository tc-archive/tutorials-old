package tuts.lsb;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;

/**
 * Created by Temple on 16/02/15.
 */
@Service
public class DatabaseLoader {

    private final TeammateRepository teammateRepository;

    @Autowired
    public DatabaseLoader(TeammateRepository teammateRepository) {
        this.teammateRepository = teammateRepository;
    }

    @PostConstruct
    private void initDatabase() {

        Teammate roy = new Teammate("Roy", "Clarkson");
        roy.setPosition("1st base");
        teammateRepository.save(roy);

        Teammate phil = new Teammate("Phil", "Webb");
        phil.setPosition("pitcher");
        teammateRepository.save(phil);
    }
}
