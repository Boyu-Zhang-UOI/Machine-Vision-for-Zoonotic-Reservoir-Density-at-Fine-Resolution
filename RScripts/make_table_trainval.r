## --- Captures, trap-nights, visits table

dat = read.csv('../Data/Trap_Data/Clean_Both_Data_By_Trap.csv')
data.table::setDT(dat)

## Calculate statistics on trapping data
summ.dat = dat[,.(totmn = sum(Trap.weight[Mna==1]),
       tottraps = sum(Trap.weight),
       nvisits = length(unique(Visit))),
    by = list(Site)]


table.text <- "\\begin{tabular}{|c|c|c|c|}\\hline%
  \\bfseries Site & \\bfseries Mn Captures & \\bfseries Trap-nights & \\bfseries Visits \\\\ \\hline \\hline"
meat = ""
for(ii in 1:nrow(summ.dat)){
    row = paste("\n ", summ.dat$Site[ii], "&", summ.dat$totmn[ii], "&", summ.dat$tottraps[ii], "&",
                summ.dat$nvisits[ii], "\\\\", sep = " ")
     meat <- paste(meat, row)
}
## Add in totals row
meat <- paste(meat, "\\hline", collapse = "")
tot.mn = sum(summ.dat$totmn)
tot.traps = sum(summ.dat$tottraps)
tot.visits = sum(summ.dat$nvisits)
row = paste("\n ","\\bfseries Total", "&", tot.mn, "&", tot.traps, "&",
            tot.visits,
            "\\\\", sep = " ")
meat <- paste(meat, row)

meat <- paste(meat, "\\hline", collapse = "")
table <- paste(table.text, meat, "\n \\end{tabular}", collapse = "")
write(table, file = 'Table_text_output/table_data_summary.txt')


## --- Train/val table with hyperparameters

## Build the latex code from CSV dataframe 

model.path = '../Figures_agg/'
dat <- read.csv(paste0(model.path, 'best_models.csv'))

dat <- dat[order(dat$train_site),]


table.text <- "  \\begin{sidewaystable}
  \\caption{Performance of boosted regression tree model on training and validation datasets. MAE stands for mean absolute error. The hyperparameters describe the maximum allowed tree depth (Max Depth), the proportion of columns sampled at each tree-fitting iteration (ColSample), the proportion of training data that is used to generate each tree (Subsample), the number of tree-fitting iterations (Iterations), and minimum loss reduction required to make a further partition on a leaf node of the tree (gamma)}\\label{tab:perf}% <===============================================
  \\newcolumntype{R}{>{\\raggedright \\arraybackslash} X}
  \\newcolumntype{S}{>{\\centering \\arraybackslash} X}
  \\newcolumntype{T}{>{\\raggedleft \\arraybackslash} X}
  \\begin{tabularx}{\\linewidth} {>{\\setlength\\hsize{.16\\hsize}}S >{\\setlength\\hsize{.16\\hsize}}S >{\\setlength\\hsize{.025\\hsize}}S >{\\setlength\\hsize{.11\\hsize}}S >{\\setlength\\hsize{.11\\hsize}}S >{\\setlength\\hsize{.11\\hsize}}S >{\\setlength\\hsize{.11\\hsize}}S  >{\\setlength\\hsize{.11\\hsize}}S >{\\setlength\\hsize{.11\\hsize}}S >{\\setlength\\hsize{.11\\hsize}}S >{\\setlength\\hsize{.18\\hsize}}S  >{\\setlength\\hsize{.18\\hsize}}S >{\\setlength\\hsize{.18\\hsize}}S >{\\setlength\\hsize{.18\\hsize}}S}  
  %\\toprule
  \\multicolumn{1}{c}{Train} & \\multicolumn{1}{c}{Validation} & \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{Train MAE} & \\multicolumn{1}{c}{Val. MAE} & \\multicolumn{4}{c}{Hyperparameters} \\\\
  \\cmidrule(l){1-1}\\cmidrule(l){2-2}\\cmidrule(l){4-4} \\cmidrule(l){5-5}\\cmidrule(l){6-10}
  & & & & & Max Depth & ColSample & Subsample & Iterations & gamma \\tabularnewline"
meat = ""

for(ii in 1:nrow(dat)){
    row = paste(paste("\n ", dat$train_site[ii], "&", dat$val_site[ii], "&", "&",
                round(dat$train_loss[ii],3), "&",
                round(dat$val_loss[ii],3), "&",
                round(dat$max_depth[ii],3), "&",
                dat$colsample_bytree[ii], "&",
                dat$subsample[ii], "&",
                dat$n_iter[ii], "&",
                dat$gamma[ii],
                sep=" "), "\\\\", sep = " ")
     meat <- paste(meat, row)
}
## Add in totals row
meat <- paste(meat, "\\hline", collapse = "")
table <- paste(table.text, meat, "\n \\end{tabularx} \n \\end{sidewaystable}", collapse = "")
write(table, file = 'Table_text_output/table_best_models.txt')


