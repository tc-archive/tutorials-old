package tuts.lsb;

import org.springframework.data.repository.CrudRepository;

import java.math.BigInteger;

/**
 * Created by Temple on 15/02/15.
 */
public interface TeamRepository extends CrudRepository<Team, BigInteger> {

}
