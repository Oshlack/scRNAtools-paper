plotToolsNumber <- function(data.counts, pal) {

    number.plot <- ggplot(date.counts, aes(x = Date, y = Total)) +
        geom_line(size = 2, colour = pal["purple"]) +
        xlab("Date") +
        ylab("Number of tools") +
        scale_x_date(breaks = scales::pretty_breaks(10)) +
        ggtitle("Increase in tools over time") +
        theme_cowplot() +
        theme(plot.title = element_text(size = 20),
              axis.title.x = element_blank(),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(angle = 90, vjust = 0.5)
        )

    return(number.plot)
}

plotPublication <- function(tools, pal) {
    pub.levels <- c("Published", "Preprint", "NotPublished")
    pub.labels <- c("Published", "Preprint", "Not Published")

    plot.data <- tools %>%
        mutate(Publications = if_else(is.na(Publications), 0L, Publications),
               Preprints = if_else(is.na(Preprints), 0L, Preprints)) %>%
        summarise(NotPublished = sum(Preprints == 0 & Publications == 0,
                                     na.rm = TRUE),
                  Published = sum(Publications > 0, na.rm = TRUE),
                  Preprint = sum(Preprints > 0 & Publications == 0,
                                 na.rm = TRUE)) %>%
        gather(key = Type, value = Count) %>%
        mutate(Type = factor(Type, levels = pub.levels,
                             labels = pub.labels)) %>%
        arrange(Type) %>%
        mutate(Cumulative = cumsum(Count),
               Midpoint = max(Cumulative) - (Cumulative - (Count / 2)),
               Label = paste0(Type, "\n",
                              round(Count / sum(Count) * 100, 1), "%"))

    pub.plot <- ggplot(plot.data, aes(x = 1, weight = Count, fill = Type)) +
        geom_bar(width = 1, position = "stack") +
        coord_polar(theta = "y") +
        geom_text(aes(x = 1.8, y = Midpoint, label = Label, colour = Type),
                  size = 6) +
        scale_fill_manual(values = unname(pal)[4:6]) +
        scale_colour_manual(values = unname(pal)[4:6]) +
        ggtitle("Publication status") +
        theme_nothing() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              legend.position = "none"
        )

    return(pub.plot)
}

plotPubTime <- function(tools, pal) {
    pub.levels <- c("Published", "Preprint", "NotPublished")
    pub.labels <- c("Published", "Preprint", "Not Published")

    plot.data <- tools %>%
        group_by(IsOld) %>%
        mutate(Publications = if_else(is.na(Publications), 0L, Publications),
               Preprints = if_else(is.na(Preprints), 0L, Preprints)) %>%
        summarise(NotPublished = sum(Preprints == 0 & Publications == 0,
                                     na.rm = TRUE),
                  Published = sum(Publications > 0, na.rm = TRUE),
                  Preprint = sum(Preprints > 0 & Publications == 0,
                                 na.rm = TRUE)) %>%
        select(IsOld, NotPublished, Published, Preprint) %>%
        gather(key = Type, value = Count, -IsOld) %>%
        group_by(IsOld) %>%
        mutate(Prop = Count / sum(Count)) %>%
        ungroup() %>%
        mutate(Type = factor(Type, levels = pub.levels, labels = pub.labels),
               IsOld = factor(IsOld, levels = c("TRUE", "FALSE")))

    pub.time.plot <- ggplot(plot.data, aes(x = Type, y = Prop, fill = IsOld)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(name = "Date added",
                          labels = c("Before 2016-10-01", "After 2016-10-01"),
                          values = unname(pal)) +
        scale_y_continuous(labels = scales::percent) +
        ggtitle("Publication status over time") +
        ylab("Percentage of tools") +
        theme_cowplot() +
        theme(legend.position = c(0.75, 0.85),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.key.size = unit(25, "points"),
              plot.title = element_text(size = 20),
              axis.text = element_text(size = 12),
              axis.title.x = element_blank()
        )
}

plotLicenses <- function(tools, pal) {
    license.levels <- c("Apache", "Artistic", "BSD", "GPL", "MIT", "Other",
                        "Unknown")

    plot.data <- tools %>%
        select(License) %>%
        mutate(IsGPL = str_detect(License, "GPL"),
               IsBSD = str_detect(License, "BSD"),
               IsMIT = str_detect(License, "MIT"),
               IsApache = str_detect(License, "Apache"),
               IsArtistic = str_detect(License, "Artistic"),
               IsUnknown = is.na(License),
               IsOther = !(IsGPL | IsBSD | IsMIT | IsApache | IsArtistic |
                               IsUnknown)) %>%
        summarise(Apache = sum(IsApache, na.rm = TRUE),
                  Artistic = sum(IsArtistic, na.rm = TRUE),
                  BSD = sum(IsBSD, na.rm = TRUE),
                  GPL = sum(IsGPL, na.rm = TRUE),
                  MIT = sum(IsMIT, na.rm = TRUE),
                  Other = sum(IsOther),
                  Unknown = sum(IsUnknown)) %>%
        gather(key = License, value = Count) %>%
        mutate(License = factor(License, levels = license.levels),
               Cumulative = cumsum(Count),
               Midpoint = Cumulative - (Count / 2),
               Label = paste0(License, "\n",
                              round(Count / sum(Count) * 100, 1), "%"))

    licenses.plot <- ggplot(plot.data,
                            aes(x = 1, weight = Count, fill = License)) +
        geom_bar(width = 1, position = "stack") +
        coord_polar(theta = "y") +
        geom_text(aes(x = 1.75, y = nrow(tools) - Midpoint, label = Label,
                      colour = License), size = 6) +
        scale_fill_manual(values = unname(pal)) +
        scale_colour_manual(values = unname(pal)) +
        ggtitle("Software licenses") +
        theme_nothing() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              legend.position = "none"
        )

    return(licenses.plot)
}

plotPlatforms <- function(tools, pal) {
    platforms.levels <- c("R", "Python", "MATLAB", "CPP", "Other")
    platforms.labels <- c("R", "Python", "MATLAB", "C++", "Other")

    plot.data <- tools %>%
        select(Platform) %>%
        mutate(IsR = str_detect(Platform, "R"),
               IsPython = str_detect(Platform, "Python"),
               IsMATLAB = str_detect(Platform, "MATLAB"),
               IsCPP = str_detect(Platform, "C++"),
               IsOther = !(IsR | IsPython | IsMATLAB | IsCPP)) %>%
        summarise(R = sum(IsR),
                  Python = sum(IsPython),
                  MATLAB = sum(IsMATLAB),
                  CPP = sum(IsCPP),
                  Other = sum(IsOther)) %>%
        gather(key = Platform, value = Count) %>%
        mutate(Platform = factor(Platform,
                                 levels = platforms.levels,
                                 labels = platforms.labels))

    platforms.plot <- ggplot(plot.data, aes(x = Platform, weight = Count,
                                            fill = Platform)) +
        geom_bar(aes(y = (..count..) / sum(..count..))) +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = unname(pal)) +
        ylab("Percentage of tools") +
        ggtitle("Platforms used by analysis tools") +
        theme_cowplot() +
        theme(axis.title.x = element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 20),
              axis.text = element_text(size = 12)
        )

    return(platforms.plot)
}

plotCategories <- function(cat.counts, pal) {
    cats.plot <- ggplot(cat.counts,
           aes(x = Category, weight = Prop, fill = Phase)) +
        geom_bar() +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = pal) +
        ylab("Percentage of tools") +
        ggtitle("Analysis categories") +
        theme_cowplot() +
        theme(axis.title.x = element_blank(),
              legend.position = c(0.85, 0.70),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.key.size = unit(25, "points"),
              plot.title = element_text(size = 20),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        )

    return(cats.plot)
}

plotCatsTime <- function(tools, cat.counts, pal) {
    cats.time.points <- tools %>%
        group_by(IsOld) %>%
        mutate(Size = n()) %>%
        group_by(IsOld, Size) %>%
        summarise_at(8:38, sum) %>%
        gather(key = Category, value = Count, -IsOld, -Size) %>%
        mutate(Category = str_replace_all(Category,
                                          "([[:upper:]])", " \\1")) %>%
        mutate(Category = str_trim(Category)) %>%
        mutate(Category = ifelse(Category == "U M Is", "UMIs", Category)) %>%
        mutate(Category = factor(Category,
                                 levels = rev(levels(cat.counts$Category)))) %>%
        group_by(IsOld) %>%
        mutate(Prop = Count / Size) %>%
        ungroup() %>%
        mutate(IsOld = factor(IsOld, levels = c(TRUE, FALSE)))

    cats.time.changes <- cats.time.points %>%
        select(Category, IsOld, Prop) %>%
        spread(IsOld, Prop) %>%
        rename(Old = `TRUE`, New = `FALSE`) %>%
        mutate(Decrease = New < Old) %>%
        mutate(Decrease = factor(Decrease, levels = c(TRUE, FALSE)))

    cats.time.plot <- ggplot(cats.time.points, aes(x = Category)) +
        geom_linerange(data = cats.time.changes,
                       aes(ymin = Old, ymax = New, colour = Decrease),
                       size = 4.5, alpha = 0.3) +
        geom_point(aes(y = Prop, colour = IsOld), size = 4) +
        scale_color_manual(name = "Date added",
                           labels = c("Before 2016-10-01", "After 2016-10-01"),
                           values = unname(pal)) +
        scale_y_continuous(labels = scales::percent) +
        coord_flip() +
        ylab("Percentage of tools") +
        ggtitle("Change in analysis categories") +
        theme_cowplot() +
        theme(axis.title.y = element_blank(),
              legend.position = c(0.7, 0.2),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.key.size = unit(25, "points"),
              plot.title = element_text(size = 20),
              axis.text = element_text(size = 12),
              plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")
        )

    return(cats.time.plot)
}

plotPhasesDate <- function(tools, data.counts, pal) {
    phase.levels <- c("Phase1", "Phase2", "Phase3", "Phase4", "Multiple",
                      "Other")
    phase.labels <- c("Phase 1", "Phase 2", "Phase 3", "Phase 4", "Multiple",
                      "Other")

    plot.data <- tools %>%
        mutate(Phase1 = Alignment | Assembly | Quantification | UMIs,
               Phase2 = CellCycle | GeneFiltering | Imputation | Normalisation |
                   QualityControl,
               Phase3 = Classification | Clustering | Ordering | RareCells |
                   StemCells,
               Phase4 = DifferentialExpression | ExpressionPatterns |
                   GeneNetworks | GeneSets | MarkerGenes | VariableGenes,
               Multiple = DimensionalityReduction | Visualisation | Interactive,
               Other = AlleleSpecific | AlternativeSplicing | Haplotypes |
                   Immune | Modality | Simulation | Transformation |
                   Variants) %>%
        select(Date = Added, Phase1, Phase2, Phase3, Phase4, Multiple,
               Other) %>%
        gather(key = Phase, value = TF, -Date) %>%
        filter(TF == TRUE) %>%
        mutate(Phase = factor(Phase, levels = phase.levels,
                              labels = phase.labels)) %>%
        select(Date, Phase) %>%
        group_by(Date, Phase) %>%
        summarise(PhaseCount = n()) %>%
        ungroup() %>%
        complete(Date = full_seq(Date, 1), Phase,
                 fill = list(PhaseCount = 0)) %>%
        group_by(Phase) %>%
        mutate(PhaseTotal = cumsum(PhaseCount)) %>%
        left_join(date.counts, by = "Date") %>%
        mutate(Prop = PhaseTotal / Total)

    phases.data.plot <- ggplot(plot.data,
                               aes(x = Date, y = Prop, colour = Phase)) +
        geom_line(size = 2) +
        scale_x_date(breaks = scales::pretty_breaks(10)) +
        scale_y_continuous(labels = scales::percent) +
        scale_colour_manual(values = pal) +
        xlab("Date") +
        ylab("Percentage of tools") +
        ggtitle("Analysis phases over time") +
        theme_cowplot() +
        theme(plot.title = element_text(size = 20),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(angle = 90, vjust = 0.5),
              axis.title.x = element_blank(),
              legend.position = "right"
        )

    return(phases.data.plot)
}

plotCatsPerTool <- function(tools, pal) {
    plot.data <- tools %>%
        select(Name, 8:38) %>%
        gather(key = Category, value = TF, -Name) %>%
        group_by(Name) %>%
        summarise(Count = sum(TF))

    cats.tools.plot <- ggplot(plot.data,
                              aes(x = factor(Count, levels = 1:14))) +
        geom_bar(fill = pal["purple"]) +
        scale_x_discrete(drop = FALSE) +
        ggtitle("Number of categories per tool") +
        xlab("Number of categories") +
        ylab("Number of tools") +
        theme_cowplot() +
        theme(plot.title = element_text(size = 20),
              axis.text = element_text(size = 12)
        )

    return(cats.tools.plot)
}

plotCatsPerToolTime <- function(tools, pal) {
    plot.data <- tools %>%
        select(Name, IsOld, 8:38) %>%
        gather(key = Category, value = TF, -Name, -IsOld) %>%
        group_by(Name, IsOld) %>%
        summarise(Cats = sum(TF)) %>%
        ungroup() %>%
        select(-Name) %>%
        group_by(IsOld, Cats) %>%
        mutate(Count = n()) %>%
        unique() %>%
        group_by(IsOld) %>%
        mutate(Prop = Count / sum(Count)) %>%
        ungroup() %>%
        complete(IsOld, Cats, fill = list(Count = 0, Prop = 0)) %>%
        mutate(IsOld = factor(IsOld, levels = c(TRUE, FALSE)))

    cats.tools.time.plot <- ggplot(plot.data,
                                   aes(x = factor(Cats, levels = 1:14),
                                       y = Prop, fill = IsOld)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(name = "Date added",
                          labels = c("Before 2016-10-01", "After 2016-10-01"),
                          values = unname(pal)) +
        scale_x_discrete(drop = FALSE) +
        scale_y_continuous(labels = scales::percent) +
        ggtitle("Categories per tool by date added") +
        xlab("Number of categories") +
        ylab("Percentage of tools") +
        theme_cowplot() +
        theme(plot.title = element_text(size = 20),
              axis.text = element_text(size = 12),
              legend.position = c(0.7, 0.7)
        )

    return(cats.tools.time.plot)
}
