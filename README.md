# utl-web-scaping-Loop-over-many-URLs-scrape-parse-extract-nodes-and-put-into-a-sas-table
Web scaping Loop over many URLs scrape parse extract nodes and put into a sas table.
    Web scaping Loop over many URLs scrape parse extract nodes and put into a sas table

    macro on end

    github
    https://tinyurl.com/y2kors3p
    https://github.com/rogerjdeangelis/utl-web-scaping-Loop-over-many-URLs-scrape-parse-extract-nodes-and-put-into-a-sas-table

    Other web scrapping repos
    https://tinyurl.com/y2mrqoo4
    https://github.com/rogerjdeangelis?utf8=%E2%9C%93&tab=repositories&q=scrape+in%3Areadme&type=&language=

    StackOverflow
    https://tinyurl.com/y69x57gm
    https://stackoverflow.com/questions/54672672/r-loop-over-each-url-scrape-parse-extract-nodes-and-put-into-a-data-frame

    Matt Jewett profile
    https://stackoverflow.com/users/7476345/matt-jewett

    macros
    https://tinyurl.com/y9nfugth
    https://github.com/rogerjdeangelis/utl-macros-used-in-many-of-rogerjdeangelis-repositories

    INPUT
    ======

    Example Page  1 of realtors in zipcode 33415

    https://www.realtor.com/realestateagents/33415/pg-1

    NOTE iterate over zip and pages               Zipcode        First page of many
                                               ==============    ==================
    https://www.realtor.com/realestateagents/      33415       /       pg-1

    This is what a one page looks like

    +====================================+
    |  +============+                    |
    |  |   33145    |                    |
    |  +============+                    |
    |                                    |
    |         @@@                        |
    |       @/- -\@    Mary Smith        |
    |      @| o o |@   Properties Inc    |
    |      @|  o  |@   (111) 999-3333    |
    |       | \_/ |                      |
    |        \___/                       |
    |                                    |
    |         WWW                        |
    |        /- -\     John Smith        |
    |       | o o |    Achme, Inc        |
    |       |  o  |    (444) 555-6666    |
    |       | \_/ |                      |
    |        \___/                       |
    |         ___                        |
    |        /- -\                       |
    |       | o o |    Mark Smith        |
    |       |  o  |    Abstract LLC      |
    |       | \_/ |    (777) 888-9999    |
    |        \___/                       |
    |                                    |
    +====================================+


    EXAMPLE OUTPUT
    --------------

    Not the the utl_rens macro allows V5 trasport to have long variable names (uses SAS labels)

    WANT_LONG_NAMES tttxl tbs=900 ( corrupted data on purpose)

    Obs AGENTID    AGENTNAME          AGENTADDR           AGENTPHONE      PHONETYPE   AGENTWEBSITE

      1 9109999    Mxrcw  Hxus        Pxlm Bzxch, FL    (991) 999-9990    Mtbwlz      http://pxulsxpzrstzwn.ctm
      9 999909     Kxthwz Mxnfrzdw    Wzllwngttn, FL    (991) 999-1911    Offwcz      http://www.HtmzRunRzxlEstxtz.ctm
      9 999991     Pxul Sxpzrstzwn    Lxntxnx, FL       (991) 909-9999    Mtbwlz      http://www.BwnkRzxlty.ctm
    ...
    199 9999999    Ashlzy Ftglzmxn    Kkkkckckkc FL
    199 1999990    Ntrmxn Fwnzmxn     Btcx Rxttn, FL    (991) 999-9999    Mtbwl       199 http://PrwmzPrtpzrtyAsstcwxtzs.ctm
    199 99901      Judwz Wwlctx       Jupwtzr, FL       (991) 910-9999    Offwcz      199 http://judwzwwlctx.cbwnttuch.ctm/
    200 909991     Htmz Rzxl Estxtz   Jupwtzr, FL       (991) 999-9999    Mtbwl       900 http://www.HtmzRunRzxlEstxtz.ctm


    PROCESS
    =======

    %utl_submit_r64(%tslit(
    library(rvest);
    library(SASxport);
    zip.codes <- c("33413", "33419");
    results <- list();
    result.index <- 0;
    for(zip in zip.codes){
      url <- paste0("https://www.realtor.com/realestateagents/", zip ,"/pg-1" );
      page <- read_html(url);
      max.pages <- as.numeric(max(page %>%
                                    html_nodes(xpath = '//*[@class="page"]') %>%
                                    html_nodes("a") %>%
                                    html_text));
      for(i in c(1:max.pages)){
        print(paste("Processing Zip Code", zip, "- Page", i, "of", max.pages));
        result.index <- result.index + 1;
        url <- paste0("https://www.realtor.com/realestateagents/", zip,"/pg-", i);
        page <- read_html(url);
        df <- data.frame(AgentID = page %>%
                     html_nodes(xpath = '//*[@id="call_inquiry_cta"]') %>%
                     xml_attr("data-agent-id"),
           AgentName = page %>%
                     html_nodes(xpath = '//*[@id="call_inquiry_cta"]') %>%
                     xml_attr("data-agent-name"),
           AgentAddr = page %>%
                     html_nodes(xpath = '//*[@id="call_inquiry_cta"]') %>%
                     xml_attr("data-agent-address"),
           AgentPhone = sub("tel:", "", page %>%
                                        html_nodes(xpath = '//*[@id="call_inquiry_cta"]') %>%
                                        xml_attr("href")),
           PhoneType = page %>%
                       html_nodes(xpath = '//*[@id="call_inquiry_cta"]') %>%
                       xml_attr("data-agent-num-type"),
           AgentWebSite = page %>%
                          html_nodes(xpath = '//*[@id="call_inquiry_cta"]') %>%
                          xml_attr("data-agent-web-url"));
        results[[result.index]] <- df;
      };
    };
    df<-do.call(rbind, results);
    want<-lapply(df,function(x) if(is.factor(x)) as.character(x) else x);
    want<-as.data.frame(want, stringsAsFactors=FALSE);
    str(want);
    label(want$AgentID     ) <-"AgentID"    ;
    label(want$AgentName   ) <-"AgentName"  ;
    label(want$AgentAddr   ) <-"AgentAddr"  ;
    label(want$AgentPhone  ) <-"AgentPhone" ;
    label(want$PhoneType   ) <-"PhoneType"  ;
    label(want$AgentWebSite) <-"AgentWebSite";
    write.xport(want,file="d:/xpt/want.xpt");
    ));

    *____    _
    |  _ \  | | ___   __ _
    | |_) | | |/ _ \ / _` |
    |  _ <  | | (_) | (_| |
    |_| \_\ |_|\___/ \__, |
                     |___/
    ;


    [1] "Processing Zip Code 33413 - Page 1 of 5"
    [1] "Processing Zip Code 33413 - Page 2 of 5"
    [1] "Processing Zip Code 33413 - Page 3 of 5"
    [1] "Processing Zip Code 33413 - Page 4 of 5"
    [1] "Processing Zip Code 33413 - Page 5 of 5"

    [1] "Processing Zip Code 33419 - Page 1 of 5"
    [1] "Processing Zip Code 33419 - Page 2 of 5"
    [1] "Processing Zip Code 33419 - Page 3 of 5"
    [1] "Processing Zip Code 33419 - Page 4 of 5"
    [1] "Processing Zip Code 33419 - Page 5 of 5"

    'data.frame':	200 obs. of  6 variables:
     $ AgentID     : chr  "2108876" "372604" "582821" "1184897" ...
     $ AgentName   : chr  "Marci  Haus" "Kathie Manfredi" "Paul Saperstein" "Home Run Real Estate, Inc" ...
     $ AgentAddr   : chr  "Palm Beach, FL" "Wellington, FL" "Lantana, FL" "Lake Worth, FL" ...
     $ AgentPhone  : chr  "(561) 379-7970" "(561) 798-1411" "(561) 408-7986" "(561) 433-3836" ...
     $ PhoneType   : chr  "Mobile" "Office" "Mobile" "Office" ...
     $ AgentWebSite: chr  "http://www.marcihaus.valoregroup.com/" "" "http://paulsaperstein.com" "http://www.HomeRunRealEstate.com" .

    *_                            _
    (_)_ __ ___  _ __   ___  _ __| |_
    | | '_ ` _ \| '_ \ / _ \| '__| __|
    | | | | | | | |_) | (_) | |  | |_
    |_|_| |_| |_| .__/ \___/|_|   \__|
                |_|
    ;


    * just in case;
    proc datasets lib=work mt=all;
      delete want;
    run;quit;

    libname xpt xport "d:/xpt/want.xpt";
    data want_long_names;  * has to be another dataset;

      %utl_rens(xpt.want);  ** renames using R produced labels;
      set want;

    run;quit;

    *
     _ __ ___   __ _  ___ _ __ ___
    | '_ ` _ \ / _` |/ __| '__/ _ \
    | | | | | | (_| | (__| | | (_) |
    |_| |_| |_|\__,_|\___|_|  \___/

    ;

    %macro utl_rens(dsn);

      if _n_=0 then do;
        rc=%sysfunc(dosubl('
            data __ren001;
               set &dsn(obs=1);
            run;quit;
            proc transpose data=__ren001 out=__ren002(drop=col1);
              var _all_;
            run;quit;
            proc sql;
              select
                catx(' ',_name_,"as",lbl) into :rens separated by ","
              from
                (
                 select
                    _name_
                   ,case
                        when (_label_ = ' ') then _name_
                        else _label_
                    end as lbl
                 from
                    __ren002
                )
           ;quit;
            proc sql;
               create
                   view %scan(&dsn,2,'.')  as
               select
                   &rens
               from
                   &dsn.
            ;quit;
        '));
        drop rc;
      end;

    %mend utl_rens;

