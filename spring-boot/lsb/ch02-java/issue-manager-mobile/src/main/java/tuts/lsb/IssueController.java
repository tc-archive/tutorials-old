package tuts.lsb;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * Created by Temple on 14/02/15.
 */
@Controller
public class IssueController {

    private IssueManager issueManager;

    @Autowired
    public IssueController(IssueManager issueManager) {

        this.issueManager = issueManager;
    }

    @RequestMapping(value = "/")
    public String index(Model model) {

        model.addAttribute("issues",
                issueManager.findOpenIssues());

        return "index";
    }
}
