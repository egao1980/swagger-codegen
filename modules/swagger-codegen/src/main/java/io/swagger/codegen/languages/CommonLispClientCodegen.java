package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Contact;
import io.swagger.models.Info;
import io.swagger.models.License;
import io.swagger.models.Swagger;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CommonLispClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected static final String PROJECT_NAME = "projectName";
    private static final String PROJECT_DESCRIPTION = "projectDescription";
    private static final String PROJECT_VERSION = "projectVersion";
    private static final String PROJECT_URL = "projectUrl";
    private static final String PROJECT_LICENSE_NAME = "projectLicenseName";
    private static final String PROJECT_LICENSE_URL = "projectLicenseUrl";
    private static final String SYSTEM_NAME = "systemName";

    protected String projectName;
    protected String projectDescription;
    protected String projectVersion;
    protected String systemName;

    protected String sourceFolder = "src";
    protected String testFolder = "tests";

    static Logger LOGGER = LoggerFactory.getLogger(CommonLispClientCodegen.class);

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "common-lisp";
    }

    public String getHelp() {
        return "Generates a common-lisp client.";
    }

    public CommonLispClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "common-lisp";
        apiTemplateFiles.put("api.mustache", ".lisp");
        apiTestTemplateFiles.put("api-test.mustache", ".lisp");
        embeddedTemplateDir = templateDir = "common-lisp-client";

        cliOptions.add(new CliOption(PROJECT_NAME,
                "name of the project (Default: generated from info.title or \"swagger-clj-client\")"));
        cliOptions.add(new CliOption(PROJECT_DESCRIPTION,
                "description of the project (Default: using info.description or \"Client library of <projectNname>\")"));
        cliOptions.add(new CliOption(PROJECT_VERSION,
                "version of the project (Default: using info.version or \"1.0.0\")"));
        cliOptions.add(new CliOption(PROJECT_URL,
                "URL of the project (Default: using info.contact.url or not included in project.clj)"));
        cliOptions.add(new CliOption(PROJECT_LICENSE_NAME,
                "name of the license the project uses (Default: using info.license.name or not included in project.clj)"));
        cliOptions.add(new CliOption(PROJECT_LICENSE_URL,
                "URL of the license the project uses (Default: using info.license.url or not included in project.clj)"));
        cliOptions.add(new CliOption(SYSTEM_NAME,
                "the base/top namespace (Default: generated from projectName)"));
    }

    @Override
    public void preprocessSwagger(Swagger swagger) {
        super.preprocessSwagger(swagger);

        if (additionalProperties.containsKey(PROJECT_NAME)) {
            projectName = ((String) additionalProperties.get(PROJECT_NAME));
        }
        if (additionalProperties.containsKey(PROJECT_DESCRIPTION)) {
            projectDescription = ((String) additionalProperties.get(PROJECT_DESCRIPTION));
        }
        if (additionalProperties.containsKey(PROJECT_VERSION)) {
            projectVersion = ((String) additionalProperties.get(PROJECT_VERSION));
        }
        if (additionalProperties.containsKey(SYSTEM_NAME)) {
            systemName = ((String) additionalProperties.get(SYSTEM_NAME));
        }

        if (swagger.getInfo() != null) {
            Info info = swagger.getInfo();
            if (projectName == null &&  info.getTitle() != null) {
                // when projectName is not specified, generate it from info.title
                projectName = dashize(info.getTitle());
            }
            if (projectVersion == null) {
                // when projectVersion is not specified, use info.version
                projectVersion = info.getVersion();
            }
            if (projectDescription == null) {
                // when projectDescription is not specified, use info.description
                projectDescription = info.getDescription();
            }

            if (info.getContact() != null) {
                Contact contact = info.getContact();
                if (additionalProperties.get(PROJECT_URL) == null) {
                    additionalProperties.put(PROJECT_URL, contact.getUrl());
                }
            }
            if (info.getLicense() != null) {
                License license = info.getLicense();
                if (additionalProperties.get(PROJECT_LICENSE_NAME) == null) {
                    additionalProperties.put(PROJECT_LICENSE_NAME, license.getName());
                }
                if (additionalProperties.get(PROJECT_LICENSE_URL) == null) {
                    additionalProperties.put(PROJECT_LICENSE_URL, license.getUrl());
                }
            }
        }

        // default values
        if (projectName == null) {
            projectName = "cl-swagger-client";
        }
        if (projectVersion == null) {
            projectVersion = "1.0.0";
        }
        if (projectDescription == null) {
            projectDescription = "Client library of " + projectName;
        }
        if (systemName == null) {
            systemName = dashize(projectName.toLowerCase());
        }

        additionalProperties.put(PROJECT_NAME, projectName);
        additionalProperties.put(PROJECT_DESCRIPTION, escapeText(projectDescription));
        additionalProperties.put(PROJECT_VERSION, projectVersion);
        additionalProperties.put(SYSTEM_NAME, systemName);


        supportingFiles.add(new SupportingFile("README.md.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("README.org.mustache", "", "README.org"));


        supportingFiles.add(new SupportingFile("api.asd.mustache", "", systemName + ".asd"));
        supportingFiles.add(new SupportingFile("package.mustache", sourceFolder, systemName + ".lisp"));
        supportingFiles.add(new SupportingFile("core.mustache", sourceFolder, "core.lisp"));
        supportingFiles.add(new SupportingFile("api-test.asd.mustache", "", systemName + "-test.asd"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
    }

    @Override
    public String sanitizeTag(String tag) {
        return tag.replaceAll("[^a-zA-Z_]+", "-");
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + "api" + File.separator + namespaceToFolder(apiPackage);
    }

    public String apiTestFileFolder() {
        return outputFolder + File.separator + testFolder + File.separator + "api" + File.separator + namespaceToFolder(apiPackage);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method/operation name (operationId) not allowed");
        }

        return dashize(sanitizeName(operationId));
    }

    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String toApiTestFilename(String name) {
        return super.toApiFilename(name);
    }

    @Override
    public String toApiName(String name) {
        return dashize(name);
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String toVarName(String name) {
        name = name.replaceAll("[^a-zA-Z0-9_-]+", ""); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        name = dashize(name);
        return name;
    }

    @Override
    public String escapeText(String input) {
        if (input == null) {
            return null;
        }
        return input.trim().replace("\\", "\\\\").replace("\"", "\\\"");
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> operations) {
        Map<String, Object> objs = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> ops = (List<CodegenOperation>) objs.get("operation");
        for (CodegenOperation op : ops) {
            // Convert httpMethod to lower case, e.g. "get", "post"
            op.httpMethod = op.httpMethod.toLowerCase();
        }
        return operations;
    }

    @SuppressWarnings("static-method")
    protected String namespaceToFolder(String ns) {
        return ns.replace(".", File.separator).replace("_", "-");
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

}
