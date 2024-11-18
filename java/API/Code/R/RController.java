package API.Code.R;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.Map;

import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.xhtmlrenderer.pdf.ITextRenderer;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.layout.Document;

@RestController
public class RController {
    private static final Logger LOGGER = LoggerFactory.getLogger(RController.class);
    private static final String API_URL = "localhost"; // Mis à jour pour utiliser l'URL de l'API
    private static final String RESOURCES_PATH = "resources";
    private static final String R_SCRIPTS_PATH = "r_scripts";
    private static final String CURRENTDIR = "/home/ubuntu/Projects/Java/API-Code-R";
    // API keys pour les systèmes appelants
    private static final String API_KEY_POWERPAGES = "jdksjS9JDQ90SDJQKsjdqkojsdokJD90S";
    private static final String API_KEY_WORDPRESS = "WordPressApiKey";
    // Correspondance entre les cultures et les clés JSON
    private static final Map<String, String> culturesMapping = new LinkedHashMap<>();

    static {
        culturesMapping.put("Avoine de printemps", "v1");
        culturesMapping.put("Avoine d'hiver", "v2");
        culturesMapping.put("Betterave industrielle", "v3");
        culturesMapping.put("Blé dur de printemps", "v4");
        culturesMapping.put("Blé dur d'hiver", "v5");
        culturesMapping.put("Blé tendre de printemps", "v6");
        culturesMapping.put("Blé tendre d'hiver et épeautre", "v7");
        culturesMapping.put("Chanvre papier", "v8");
        culturesMapping.put("Colza grain de printemps et navette", "v9");
        culturesMapping.put("Colza grain d'hiver", "v10");
        culturesMapping.put("Féveroles et fèves", "v11");
        culturesMapping.put("Haricots à écosser et demi-secs (grain)", "v12");
        culturesMapping.put("Haricots secs (y compris semences)", "v13");
        culturesMapping.put("Lentilles (y compris semences)", "v14");
        culturesMapping.put("Lin oléagineux", "v15");
        culturesMapping.put("Lin textile", "v16");
        culturesMapping.put("Lupin doux", "v17");
        culturesMapping.put("Maïs doux", "v18");
        culturesMapping.put("Maïs grain irrigué", "v19");
        culturesMapping.put("Maïs grain non irrigué", "v20");
        culturesMapping.put("Maïs semence", "v21");
        culturesMapping.put("Oignon blanc", "v22");
        culturesMapping.put("Oignon de couleur", "v23");
        culturesMapping.put("Orge de printemps", "v24");
        culturesMapping.put("Orge d'hiver et escourgeon", "v25");
        culturesMapping.put("Pois protéagineux", "v26");
        culturesMapping.put("Pois secs", "v27");
        culturesMapping.put("Pommes de terre de conservation ou demi-saison", "v28");
        culturesMapping.put("Pommes de terre de féculerie", "v29");
        culturesMapping.put("Seigle et méteil", "v30");
        culturesMapping.put("Soja", "v31");
        culturesMapping.put("Sorgho grain", "v32");
        culturesMapping.put("Tournesol", "v33");
        culturesMapping.put("Triticale", "v34");
    }

    // Méthode pour valider l'API key et identifier le système appelant
    private String validateApiKey(String apiKey) {
        if (API_KEY_POWERPAGES.equals(apiKey)) {
            return "PowerPages";
        } else if (API_KEY_WORDPRESS.equals(apiKey)) {
            return "WordPress";
        } else {
            return null; // Clé invalide
        }
    }

    @RequestMapping("/runRServe")
    public String runRServe() {
        try {
            startRserve();
            LOGGER.info("Rserve started successfully.");
            return "Rserve started successfully.";
        } catch (IOException e) {
            LOGGER.error(String.format("Failed to start Rserve: %s", e.getMessage()), e);
            return "Failed to start Rserve: " + e.getMessage();
        }
    }

    private void startRserve() throws IOException {
        String[] command = { "R", "CMD", "Rserve", "--no-save" };
        ProcessBuilder pb = new ProcessBuilder(command);
        pb.redirectErrorStream(true);
        Process process = pb.start();

        new Thread(() -> {
            try (var reader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    if (LOGGER.isInfoEnabled()) {
                        LOGGER.info(String.format("Rserve output: %s", line));
                    }
                }
            } catch (IOException e) {
                LOGGER.error(String.format("Error reading Rserve output: %s", e.getMessage()), e);
            }
        }).start();
    }

    @GetMapping("/api/installPackages")
    public String installPackages() {
        RConnection connection = null;
        try {
            connection = new RConnection(API_URL); // Utilisation de l'URL de l'API RServe distante
            if (LOGGER.isInfoEnabled()) {
                LOGGER.info("Connected to Rserve for package installation.");
            }

            String[] packages = { "readxl", "writexl", "dplyr", "tidyr", "mclust", "ggplot2", "stats", "gridExtra",
                    "jsonlite" };

            for (String pkg : packages) {
                if (LOGGER.isInfoEnabled()) {
                    LOGGER.info(String.format("Installing package: %s", pkg));
                }
                connection.eval("if (!require('" + pkg + "', quietly = TRUE)) install.packages('" + pkg + "')");
            }

            LOGGER.info("R packages installed successfully.");
            return "R packages installed successfully.";
        } catch (RserveException e) {
            LOGGER.error(String.format("Rserve Exception during package installation: %s", e.getMessage()), e);
            return "Error installing R packages: " + e.getMessage();
        } finally {
            if (connection != null) {
                connection.close();
            }
        }
    }

    @PostMapping(value = "/api/process", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)

    public ResponseEntity<String> processJson(@RequestBody String inputJson) {
        RConnection connection = null;
        try {
            // Lire l'entrée JSON
            ObjectMapper mapper = new ObjectMapper();
            JsonNode input = mapper.readTree(inputJson);

            // Vérification de l'API key
            String apiKey = input.get("apikey").asText();
            String system = validateApiKey(apiKey);
            if (system == null) {
                return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body("{\"error\": \"Invalid API key.\"}");
            }

            // Préparation des valeurs des cultures
            Map<String, Object> cultureValues = new LinkedHashMap<>();
            cultureValues.put("apikey", apiKey);
            cultureValues.put("dpt", Integer.parseInt(input.get("departement").asText()));

            // Initialisation des cultures à 0
            for (String vKey : culturesMapping.values()) {
                cultureValues.put(vKey, 0);
            }

            // Assignation des valeurs du JSON d'entrée aux bonnes cultures
            input.fields().forEachRemaining(field -> {
                String cultureName = field.getKey();
                if (culturesMapping.containsKey(cultureName)) {
                    String vKey = culturesMapping.get(cultureName);
                    cultureValues.put(vKey, field.getValue().asDouble());
                }
            });

            // Créez le JSON d'entrée selon le format désiré
            ObjectMapper objectMapper = new ObjectMapper();
            String cultureValuesJson = objectMapper.writeValueAsString(cultureValues);

            // Écrire le fichier JSON d'entrée pour le script R
            String inputFilePath = CURRENTDIR + File.separator + "src" + File.separator + "main" + File.separator
                    + RESOURCES_PATH + File.separator + R_SCRIPTS_PATH + File.separator + "input_data.json";
            Files.write(Paths.get(inputFilePath), cultureValuesJson.getBytes(StandardCharsets.UTF_8));

            // Appeler le script R
            connection = new RConnection(API_URL);
            LOGGER.info("Connected to Rserve for processing.");

            // Exécuter le script R
            String scriptPath = CURRENTDIR + File.separator + "src" + File.separator + "main" + File.separator
                    + RESOURCES_PATH + File.separator + R_SCRIPTS_PATH + File.separator + "Polarisk.R";
            connection.voidEval("source('" + scriptPath.replace("\\", "/") + "')");

            // Lire les fichiers de sortie JSON
            Map<String, Object> combinedJson = readAndProcessOutputFiles(CURRENTDIR, system, mapper);

            // Générer et inclure le fichier PDF
            ResponseEntity<String> pdfFile = processHtmlToPdfResponse(inputJson);
            combinedJson.put("file", pdfFile);

            // Retourner le résultat en fonction du système appelant
            String combinedOutputJson = mapper.writeValueAsString(combinedJson);
            return ResponseEntity.ok(combinedOutputJson);

        } catch (IOException | RserveException e) {
            LOGGER.error("Error during processing: ", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("{\"error\": \"Erreur lors du traitement des fichiers JSON.\"}");
        } finally {
            if (connection != null) {
                connection.close();
            }
        }
    }

    // Méthode pour lire et traiter les fichiers de sortie

    private Map<String, Object> readAndProcessOutputFiles(String CURRENTDIR, String system, ObjectMapper mapper)
            throws IOException {
        Map<String, Object> combinedJson = new LinkedHashMap<>();
        String[] outputFileNames = {
                "input_data.json",
                "output_risques.json",
                "output_risque_marche.json",
                "output_ca_assurance.json",
                "output_ca_brut_et_risques.json",
                "output_pertes.json",
                "Moyenne_anuelle_5_ans.json",
                "Moyenne_protelis_5_ans.json"
        };

        for (String outputFileName : outputFileNames) {
            String outputFilePath = CURRENTDIR + File.separator + "src" + File.separator + "main" + File.separator
                    + RESOURCES_PATH + File.separator + R_SCRIPTS_PATH + File.separator + outputFileName;
            File outputFile = new File(outputFilePath);
            if (outputFile.exists()) {
                byte[] fileBytes = Files.readAllBytes(Paths.get(outputFilePath));
                String jsonContent = new String(fileBytes, StandardCharsets.UTF_8);

                // Limiter les données pour WordPress
                if ("WordPress".equals(system)) {
                    jsonContent = limitDataForWordPress(jsonContent);
                }

                combinedJson.put(outputFileName, mapper.readTree(jsonContent));
            }
        }

        return combinedJson;
    }

    // Méthode pour limiter les données pour WordPress
    private String limitDataForWordPress(String jsonContent) {
        // Implémenter la logique pour restreindre les résultats ici
        // Par exemple, supprimer certains champs ou réduire la granularité des données
        return jsonContent; // Par défaut, retourner les données complètes
    }

    public ResponseEntity<String> processHtmlToPdfResponse(@RequestBody String inputJson) {
        try {
            // Lire et traiter le fichier HTML
            String htmlFilePath = CURRENTDIR + File.separator + "src" + File.separator + "main" + File.separator
                    + "resources" + File.separator + R_SCRIPTS_PATH + File.separator + "final.html";
            String htmlContent = Files.readString(Paths.get(htmlFilePath), StandardCharsets.UTF_8);

            // Générer le PDF à partir du HTML
            Map<String, Object> pdfFile = generatePdfFromHtml(htmlContent);

            // Créer la réponse JSON
            ObjectMapper mapper = new ObjectMapper();
            String jsonResponse = mapper.writeValueAsString(pdfFile);
            return ResponseEntity.ok(jsonResponse);

        } catch (IOException e) {
            LOGGER.error("Erreur lors de la génération du fichier HTML ", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("{\"error\": \"Erreur lors de la génération du fichier HTML.\"}");
        }
    }

    private Map<String, Object> generatePdfFromHtml(String htmlContent) throws IOException {
        Map<String, Object> pdfFile = new LinkedHashMap<>();
        String pdfFilePath = CURRENTDIR + File.separator + "src" + File.separator + "main" + File.separator
                + "resources" + File.separator + R_SCRIPTS_PATH + File.separator + "finalReport.pdf";

        // Utiliser PdfWriter et PdfDocument
        try (PdfWriter writer = new PdfWriter(pdfFilePath);
                PdfDocument pdfDocument = new PdfDocument(writer);
                Document document = new Document(pdfDocument)) {

            // Ajouter une page manuellement avant de commencer le rendu
            pdfDocument.addNewPage(); // Ajout explicite d'une page

            // Utiliser ITextRenderer pour convertir le contenu HTML en PDF
            ITextRenderer renderer = new ITextRenderer();
            renderer.setDocumentFromString(htmlContent);
            renderer.layout();

            // Rendre le PDF en utilisant ITextRenderer
            renderer.createPDF(writer, false);
            renderer.finishPDF();
        } catch (Exception e) {
            LOGGER.error("Erreur lors de la génération du PDF à partir du HTML", e);
            throw new IOException("Erreur lors de la génération du fichier PDF à partir du HTML.", e);
        }

        // Lire et encoder le fichier PDF en base64
        byte[] pdfBytes = Files.readAllBytes(Paths.get(pdfFilePath));
        String encodedPdf = Base64.getEncoder().encodeToString(pdfBytes);

        // Ajouter les informations du PDF à la réponse
        pdfFile.put("filename", "GeneratedReport.pdf");
        pdfFile.put("filetype", "application/pdf");
        pdfFile.put("filecontent", encodedPdf);

        return pdfFile;
    }

    @GetMapping("/api/testRConnection")
    public ResponseEntity<String> testRConnection() {
        RConnection connection = null;
        try {
            connection = new RConnection(API_URL); // Mise à jour de l'URL pour l'API distante
            LOGGER.info("Testing Rserve connection.");

            String rVersion = connection.eval("R.version.string").asString();
            if (LOGGER.isInfoEnabled()) {
                LOGGER.info(String.format("Rserve connected. R version: %s", rVersion));
            }
            return ResponseEntity.ok("Connected to RServe. R version: " + rVersion);
        } catch (RserveException | REXPMismatchException e) {
            LOGGER.error(String.format("Error connecting to RServe: %s", e.getMessage()), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("Error connecting to RServe: " + e.getMessage());
        } finally {
            if (connection != null) {
                connection.close();
            }
        }
    }
}