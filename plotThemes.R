library(data.table)
library(ggplot2)

thisPath="./themes/"
colourFiles=list.files(thisPath)

getColourData=function(thisI){
  #awk out the theme elements and hex values
  thisAwk=
    paste0("awk '/set/{print $2 \",\" $3}' ","'",thisPath,colourFiles[thisI],"'")
  
  thisColourTheme=fread(input = thisAwk,header = FALSE)
  names(thisColourTheme) = c("element","hex")
  thisColourTheme$theme=colourFiles[thisI]
  
  return(thisColourTheme)
}
getColourData(1)
plotData=rbindlist(lapply(X = 1:length(colourFiles),FUN = getColourData))

ggplot(data = plotData
       ,mapping = aes(x = theme
                      ,y = element
                      ,fill = hex)
)+
  geom_tile() +
  geom_text(aes(label = hex)) +
  scale_fill_identity()


#Compare two themes
diffThemeA='solarized_dark'
diffThemeB='solarized_light'

wideData=dcast(plotData[theme==diffThemeA|theme==diffThemeB], element ~ theme,value.var = 'hex',fun.aggregate = function(x){x[1]})

#Grab rows where the two themes have different hex values
wideData=wideData[eval(parse(text = diffThemeA))!=eval(parse(text =diffThemeB))]
diffPlotData=melt(wideData,id.vars = 'element',variable.name = 'theme',value.name = 'hex')

ggplot(data = diffPlotData
       ,mapping = aes(x = theme
                      ,y = element
                      ,fill = hex)
)+
  geom_tile() +
  geom_text(aes(label = hex)) +
  scale_fill_identity()
