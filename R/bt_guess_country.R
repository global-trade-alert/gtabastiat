#' Country guesser: guesses country name contained in a string.
#' Not perfect - biased towards more common countries, e.g. may return 'United Kingdom' when string contains 'British Columbia' etc.
#' 
#' The results are in alphabetical order, not the order they appear in the string.
#' 
#' @param tgt.string vector of string containing country name(s)
#'
#' @return vector of the GTA names of all matches in alphabetical order.
#'
#' @examples
#' tgt.string.vect = c("In Britain, postman delivers fancy dress joy to isolating residents", "UK man turns out to be an idiot", "Something afoot in Japan")
#' for(tgt.string in tgt.string.vect){
#' print(bt_guess_country(tgt.string))
#' }
#' 
#' Usage example:
#' 
#' for (i in 1:nrow(update.table)){
#' guessed.country = bt_guess_country(update.table$art.description.en[i])
#' if(length(guessed.country)){
#'   update.table$art.country[i] = guessed.country
#' } else {
#'   update.table$art.country[i] = "Unknown"
#' }
#' }
#' 
#' @references www.globaltradealert.org
#' @Author Callum Campbell for Global Trade Alert.
#' 

bt_guess_country = function(tgt.string){
  
  
  
  #this is included for reference. it's what I used to construct the
  #countries.matcher dataframe. each regex is written by hand, so if the
  #countries list changes, this needs to be updated.
  
  #I recommend not reflowing this comment. Also with my rstudio the cursor
  #position is not accurate when you have one line that's really long, like the
  #below, so bear that in mind if you are editing it.

  
  
  # countries.matcher = data.frame(gta.name = c("Afghanistan","Albania","Algeria","American Samoa","Andorra","Angola","Antigua & Barbuda","Azerbaijan","Argentina","Australia","Austria","Bahamas","Bahrain","Bangladesh","Armenia","Barbados","Belgium","Bermuda","Bhutan","Bolivia","Bosnia & Herzegovina","Botswana","Brazil","Belize","Solomon Islands","British Virgin Islands","Brunei Darussalam","Bulgaria","Myanmar","Burundi","Belarus","Cambodia","Cameroon","Canada","Cape Verde","Cayman Islands","Central African Republic","Sri Lanka","Chad","Chile","China","Chinese Taipei","Colombia","Comoros","Mayotte","Congo","DR Congo","Cook Islands","Costa Rica","Croatia","Cuba","Cyprus","Czechia","Benin","Denmark","Dominica","Dominican Republic","Ecuador","El Salvador","Equatorial Guinea","Ethiopia","Eritrea","Estonia","Faeroe Islands","Falkland Islands","Fiji","Finland","France","French Guiana","French Polynesia","Djibouti","Gabon","Georgia","Gambia","State of Palestine","Germany","Ghana","Kiribati","Greece","Greenland","Grenada","Guadeloupe","Guam","Guatemala","Guinea","Guyana","Haiti","Vatican","Honduras","Hong Kong","Hungary","Iceland","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Ivory Coast","Jamaica","Japan","Kazakhstan","Jordan","Kenya","DPR Korea","Republic of Korea","Kuwait","Kyrgyzstan","Lao","Lebanon","Lesotho","Latvia","Liberia","Libya","Liechtenstein","Lithuania","Luxembourg","Macao","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Martinique","Mauritania","Mauritius","Mexico","Monaco","Mongolia","Republic of Moldova","Montenegro","Montserrat","Morocco","Mozambique","Oman","Namibia","Nauru","Nepal","Netherlands","Netherlands Antilles","Aruba","New Caledonia","Vanuatu","New Zealand","Nicaragua","Niger","Nigeria","Niue","Norfolk Island","Norway","Northern Mariana Islands","Micronesia","Marshall Islands","Palau","Pakistan","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Pitcairn","Poland","Portugal","Guinea-Bissau","Timor-Leste","Puerto Rico","Qatar","Reunion","Romania","Russia","Rwanda","Saint-Barthelemy","Saint Helena","Saint Kitts & Nevis","Anguilla","Saint Lucia","Saint-Martin","Saint Pierre & Miquelon","Saint Vincent & the Grenadines","San Marino","Sao Tome & Principe","Saudi Arabia","Senegal","Serbia","Seychelles","Sierra Leone","India","Singapore","Slovakia","Vietnam","Slovenia","Somalia","South Africa","Zimbabwe","Spain","South Sudan","Republic of the Sudan","Western Sahara","Suriname","Svalbard & Jan Mayen Islands","Swaziland","Sweden","Switzerland","Syria","Tajikistan","Thailand","Togo","Tokelau","Tonga","Trinidad & Tobago","United Arab Emirates","Tunisia","Turkey","Turkmenistan","Turks & Caicos Islands","Tuvalu","Uganda","Ukraine","Macedonia","Egypt","United Kingdom","Tanzania","United States of America","US Virgin Islands","Burkina Faso","Uruguay","Uzbekistan","Venezuela","Wallis & Futuna Islands","Samoa","Yemen","Zambia"),
  #                                name.part = c("afgha","albania","algeria","american samoa","andorr","angola","antigua","azerbaija","argentin","australia","austria","bahama","bahrai","banglades","armeni","barbado","belgiu","bermud","bhuta","bolivi","bosnia","botswan","brazi","beliz","solomon island","british virgin","brunei","bulgari","(myanma)|(burma)","burund","belaru","cambodi","cameroo","canada","(cape verde)|(cabo verde)","cayman island","central african rep","sri lank","(chad\\W)|(^chad$)","(chile\\W)|(^chile$)","(china\\W)|(^china$)|(prc\\w)|(^prc$)","(taipei)|(taiwan)","colombia","comoro","mayotte","congo","(dr congo)|(democratic republic of congo)","cook island","costa rica","croati","cuba\\W","cyprus","czech","benin","denmark","dominica(?!n)","dominican republi","ecuador","el salvador","equatorial guine","ethiopi","eritrea","estonia","faeroe","falkland island","fiji","finland","france","french gu","french polynesi","djibout","gabon","georgia","gambia","palestin","germany","ghana","kiribati","greece","greenland","grenada","guadeloupe","guam","guatemala","guinea(?!(-b)|( bis))","guyan","hait","vatican","hondura","hong kon","hungary","iceland","indonesia","iran","iraq","ireland","israel","italy","(ivory coas)|(c.{1,3}te d'ivoire)","jamaica","japan","kazakhstan","jordan","kenya","(dprk)|(north korea)|(dpr korea)","(south korea)|(republic of korea)","kuwait","kyrgyz","lao","lebanon","lesotho","latvia","liberia","libya","liechtenstein","lithuania","(luxembourg)|(europe)|(\\Weu\\W)|(^eu$)","maca[uo]","madagascar","malawi","malaysia","maldive","(mali\\W)|(^mali$)","malta","martinique","mauritani","mauritius","mexico","monaco","mongoli","moldova","montenegr","montserra","morocco","mozambiqu","(oman\\W)|(^oman$)","namibia","nauru","nepal","netherland","netherlands antille","(aruba\\W)|(^aruba$)","new caledonia","vanuatu","new zealand","nicaragua","(niger\\W)|(niger$)","nigeria","(niue\\W)|(^niue$)","norfolk islan","norway","mariana","micronesia","marshall island","palau","pakistan","panama","(\\Wpng\\W)|(papua)|(^png$)","paraguay","(peru\\W)|(^peru$)","philippine","pitcairn","poland","portugal","bissau","(east timor)|(timor-lest)","puerto ric","qatar","reunion","romani","russia","rwanda","saint-barthelem","saint helen","kitts","anguill","\\Wlucia","saint-marti","miquelon","vincent","san marino","(principe\\W)|(principe$)","saudi arabi","senegal","(serbia\\W)|(serbia$)","seychelle","sierra leon","india","singapor","slovaki","viet ?nam","sloveni","somali","south afric","zimbabw","(spain\\W)|(spain$)","south suda","republic of the suda","western sahar","surinam","svalbard","swazilan","sweden","switzerlan","(syria\\W)|(^syria$)","tajikista","thailan","(togo\\W)|(^togo$)","tokela","(tonga\\W)|(^tonga$)","trinidad","(^uae$)|(uae\\W)|(emirates)","tunisia","turkey","turkmenistan","caicos","tuvalu","uganda","ukrain","macedonia","egypt","(uk\\W)|(britain)|(united kingdo)","tanzani","(^usa$)|(^us$)|(usa\\W)|(us\\W)|(united states)","us virgin is","burkina fas","uruguay","uzbekistan","venezuela","futuna","(samoa\\W)|(^samoa$)","yemen","zambia"),
  #                                stringsAsFactors = F)
  # 
  # save(countries.matcher, file = "R help files/countries_matcher.Rdata")


  #this is basically what the sapply() below is doing. sapply() is much more efficient.
  # matches.vect = c()
  # for(j in 1:nrow(countries.matcher)){
  #   matches.vect = c(matches.vect, grepl(countries.matcher$name.part[j], tgt.string.vect, ignore.case = T, perl = T))
  #   
  # }
  
  load(file = "R help files/countries_matcher.Rdata")
  
  #this looks weird because the syntax of sapply() and grepl() mean that the 'y'
  #variable in the sapply() is used as the 'x' variable in the grepl()
  
  matches.vect = sapply(countries.matcher$name.part, 
                        function(x, y) grepl(pattern = x, x = y, 
                                             ignore.case = T, 
                                             perl = T), 
                        y=tgt.string) %>%
    as.logical()

  
  return(countries.matcher$gta.name[matches.vect])
  
  
}


