# Vérifications de pea_notebook.qmd
pea_notebook <- parse_rmd("../../pea_notebook.qmd",
  allow_incomplete = TRUE, parse_yaml = TRUE)

test_that("Le bloc-notes est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("pea_notebook.qmd"))
  # La version compilée HTML du carnet de notes est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.

  expect_true(is_rendered_current("pea_notebook.qmd"))
  # La version compilée HTML du carnet de notes existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document est-elle conservée ?", {
  expect_true(all(c("Génétique du pois", "Génétique du pois de senteur",
    "Indépendance de la couleur et de la forme du grain de pollen")
    %in% (rmd_node_sections(pea_notebook) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes ne sont pas toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).

  expect_true(all(c("setup", "peatab", "peatabchi2", "peatabchi2comment",
    "peatab2", "peatab2chi2", "peatab2chi2comment", "peatab3", "peatab3chi2",
    "peatab3chi2comment", "peatab4", "peatab4chi2", "peatab4chi2comment")
    %in% rmd_node_label(pea_notebook)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).

  expect_true(any(duplicated(rmd_node_label(pea_notebook))))
  # Un ou plusieurs labels de chunks sont dupliqués dans le bloc-notes
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété dans le bloc-notes ?", {
  expect_true(pea_notebook[[1]]$author != "___")
  expect_true(!grepl("__", pea_notebook[[1]]$author))
  expect_true(grepl("^[^_]....+", pea_notebook[[1]]$author))
  # Le nom de l'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer vos noms dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.

  expect_true(grepl("[a-z]", pea_notebook[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteurs
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.

  expect_true(grepl("[A-Z]", pea_notebook[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteurs
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Le premier test Chi2 pour les pois est-il réalisé ?", {
  expect_true(!(rmd_select(pea_notebook, by_section("Génétique du pois")) |>
      as_document() |> grepl("^- +H~0~: +\\.\\.\\.+ *$", x = _) |> any()))
  # L'hypothèse nulle n'est pas remplie pour le premier test Chi2
  # Remplacez les "..." par vos phrases de commentaires libres (à noter que
  # le contenu de cette section n'est pas évalué automatiquement, mais il le
  # sera par vos enseignants).

  expect_true(!(rmd_select(pea_notebook, by_section("Génétique du pois")) |>
      as_document() |> grepl("^- +H~1~: +\\.\\.\\.+ *$", x = _) |> any()))
  # L'hypothèse alternative n'est pas remplie pour le premier test Chi2
  # Remplacez les "..." par vos phrases de commentaires libres (à noter que
  # le contenu de cette section n'est pas évalué automatiquement, mais il le
  # sera par vos enseignants).

  expect_true(is_identical_to_ref("peatab"))
  # Le tableau de contingence n'est pas correct ou pas réalisé dans le chunk
  # 'peatab'
  # Vérifiez votre code. Vous devez utilise as.table().

  expect_true(is_identical_to_ref("peatabchi2"))
  # Le test Chi2 n'est pas correct ou pas réalisé dans le chunk 'peatabchi2'
  # Vérifiez votre code. Vous devez utilise chisq.test() et spécifier
  # correctement les valeurs sous H0 dans p=.
  # Utilisez rescale.p = FALSE ici.

  expect_true(is_identical_to_ref("peatabchi2comment"))
  # L'interprétation du test chi2 est (partiellement) fausse dans le chunk
  # 'peatabchi2comment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Le second test Chi2 pour les pois est-il réalisé ?", {
  expect_true(is_identical_to_ref("peatab2"))
  # Le tableau de contingence n'est pas correct ou pas réalisé dans le chunk
  # 'peatab2'
  # Vérifiez votre code. Vous devez utilise as.table().

  expect_true(is_identical_to_ref("peatab2chi2"))
  # Le test Chi2 n'est pas correct ou pas réalisé dans le chunk 'peatab2chi2'
  # Vérifiez votre code. Vous devez utilise chisq.test() et spécifier
  # correctement les valeurs sous H0 dans p=.
  # Utilisez rescale.p = FALSE ici.

  expect_true(is_identical_to_ref("peatab2chi2comment"))
  # L'interprétation du test chi2 est (partiellement) fausse dans le chunk
  # 'peatab2chi2comment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Le premier test Chi2 pour les pois de senteur est-il réalisé ?", {
  expect_true(!(rmd_select(pea_notebook, by_section("Génétique du pois de senteur")) |>
      as_document() |> grepl("^- +H~0~: +\\.\\.\\.+ *$", x = _) |> any()))
  # L'hypothèse nulle n'est pas remplie pour le test Chi2
  # Remplacez les "..." par vos phrases de commentaires libres (à noter que
  # le contenu de cette section n'est pas évalué automatiquement, mais il le
  # sera par vos enseignants).

  expect_true(!(rmd_select(pea_notebook, by_section("Génétique du pois de senteur")) |>
      as_document() |> grepl("^- +H~1~: +\\.\\.\\.+ *$", x = _) |> any()))
  # L'hypothèse alternative n'est pas remplie pour le test Chi2
  # Remplacez les "..." par vos phrases de commentaires libres (à noter que
  # le contenu de cette section n'est pas évalué automatiquement, mais il le
  # sera par vos enseignants).

  expect_true(is_identical_to_ref("peatab3"))
  # Le tableau de contingence n'est pas correct ou pas réalisé dans le chunk
  # 'peatab3'
  # Vérifiez votre code. Vous devez utilise as.table().

  expect_true(is_identical_to_ref("peatab3chi2"))
  # Le test Chi2 n'est pas correct ou pas réalisé dans le chunk 'peatab3chi2'
  # Vérifiez votre code. Vous devez utilise chisq.test() et spécifier
  # correctement les valeurs sous H0 dans p=.
  # Utilisez rescale.p = TRUE ici.

  expect_true(is_identical_to_ref("peatab3chi2comment"))
  # L'interprétation du test chi2 est (partiellement) fausse dans le chunk
  # 'peatab3chi2comment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Le test Chi2 d'indépendance pour les pois de senteur est-il réalisé ?", {
  expect_true(!(rmd_select(pea_notebook, by_section("Indépendance de la couleur et de la forme du grain de pollen")) |>
      as_document() |> grepl("^- +H~0~: +\\.\\.\\.+ *$", x = _) |> any()))
  # L'hypothèse nulle n'est pas remplie pour le test Chi2 d'indépendance
  # Remplacez les "..." par vos phrases de commentaires libres (à noter que
  # le contenu de cette section n'est pas évalué automatiquement, mais il le
  # sera par vos enseignants).

  expect_true(!(rmd_select(pea_notebook, by_section("Indépendance de la couleur et de la forme du grain de pollen")) |>
      as_document() |> grepl("^- +H~1~: +\\.\\.\\.+ *$", x = _) |> any()))
  # L'hypothèse alternative n'est pas remplie pour le test Chi2 d'indépendance
  # Remplacez les "..." par vos phrases de commentaires libres (à noter que
  # le contenu de cette section n'est pas évalué automatiquement, mais il le
  # sera par vos enseignants).

  expect_true(is_identical_to_ref("peatab4"))
  # Le tableau de contingence n'est pas correct ou pas réalisé dans le chunk
  # 'peatab'
  # Vérifiez votre code. Vous devez créer un data frame avec les colonnes
  # couleur, pollen et freq.
  # Ensuite, vous utilisez xtabs() pour créer le tableau de contingence à
  # double entrée.

  expect_true(is_identical_to_ref("peatab4chi2"))
  # Le test Chi2 n'est pas correct ou pas réalisé dans le chunk 'peatab4chi2'
  # Vérifiez votre code. Vous devez utilise chisq.test() dans sa forme de test
  # Chi2 d'indépendance. Placez-le dans `pea4_chi2`.
  # Extrayez aussi les fréquences attendues de vote objet `pea4_chi2`.

  expect_true(is_identical_to_ref("peatab4chi2comment"))
  # L'interprétation du test chi2 est (partiellement) fausse dans le chunk
  # 'peatab4chi2comment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})
