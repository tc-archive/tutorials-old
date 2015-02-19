package tuts.lsb;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.social.github.api.GitHubIssue;
import org.springframework.social.github.api.impl.GitHubTemplate;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by Temple on 14/02/15.
 */
@Service
public class IssueManager implements InitializingBean {

    @Value("${github.token}")
    String githubToken;

    @Value("${org}")
    String org;

    @Value("${repos}")
    String[] repos;

    GitHubTemplate gitHubTemplate;


    @Override
    public void afterPropertiesSet() throws Exception {
        this.gitHubTemplate = new GitHubTemplate(githubToken);
    }

    public List<Issue> findOpenIssues() {

        List<Issue> openIssues = new ArrayList<>();

        for (String repo : repos) {
            final List<GitHubIssue> issues =
                    gitHubTemplate.repoOperations().getIssues(org, repo);

            openIssues.addAll(
                    issues.stream()
                    .filter(issue -> issue.getState().equals("open"))
                    .map(issue -> new Issue(repo, issue))
                    .collect(Collectors.toList())
                );


        }

        return openIssues;
    }
}
