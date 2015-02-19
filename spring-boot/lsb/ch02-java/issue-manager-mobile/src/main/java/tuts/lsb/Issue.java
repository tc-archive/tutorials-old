package tuts.lsb;

import org.springframework.social.github.api.GitHubIssue;

/**
 * Created by Temple on 14/02/15.
 */
public class Issue {

    private String repo;
    private GitHubIssue githubIssue;

    public Issue(String repo, GitHubIssue gitHubIssue) {
        this.repo = repo;
        this.githubIssue = gitHubIssue;
    }

    public String getRepo() {

        return repo;
    }

    public GitHubIssue getGithubIssue() {

        return githubIssue;
    }

}
