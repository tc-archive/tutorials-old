package tuts.lsb;

import junit.framework.TestCase;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(classes = IssueManagerApplication.class)
public class IssueManagerTest extends TestCase {


    @Autowired
    IssueManager issueManager;

    @Test
    public void findOpenIssues() {

        final List<Issue> openIssues = this.issueManager.findOpenIssues();

        assertNotNull(openIssues);

    }

}