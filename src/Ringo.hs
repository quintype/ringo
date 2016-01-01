{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ringo
       ( -- | The examples in this module assume the following code has been run.
         --   The :{ and :} will only work in GHCi.

         -- $setup
         module Ringo.Types
       , extractFactTable
       , extractDimensionTables
       , extractDependencies
       , tableDefnSQL
       , factTableDefnSQL
       , dimensionTablePopulateSQL
       , factTablePopulateSQL
       , validateTable
       , validateFact
       ) where

import Control.Monad.Reader (runReader)
import Data.Text (Text)

import Ringo.Types
import qualified Ringo.Extractor as E
import qualified Ringo.Generator as G
import qualified Ringo.Validator as V

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Ringo
-- >>> import qualified Data.Map as Map
-- >>> import qualified Data.Text as Text
-- >>> import Text.Show.Pretty
-- >>> :{
--let sessionEventsTable =
--      Table { tableName = "session_events"
--            , tableColumns =
--              [ Column "id" "uuid"                                                   NotNull
--              , Column "created_at" "timestamp without time zone"                    Null
--              , Column "member_id" "integer"                                         Null
--              , Column "publisher_id" "integer"                                      NotNull
--              , Column "user_agent" "character varying(1024)"                        Null
--              , Column "browser_name" "character varying(50)"                        Null
--              , Column "os" "character varying(50)"                                  Null
--              , Column "geo_country_name" "character varying(50)"                    Null
--              , Column "geo_city_name" "character varying(50)"                       Null
--              , Column "geo_continent_name" "character varying(15)"                  Null
--              , Column "geo_most_specific_subdivision_name" "character varying(100)" Null
--              , Column "geo_longitude" "numeric(9,6)"                                Null
--              , Column "geo_latitude" "numeric(9,6)"                                 Null
--              , Column "geo_time_zone" "character varying(20)"                       Null
--              , Column "geo_postal_code" "character varying(20)"                     Null
--              , Column "user_agent_name" "character varying(100)"                    Null
--              , Column "user_agent_type" "character varying(15)"                     Null
--              , Column "user_agent_version" "character varying(100)"                 Null
--              , Column "user_agent_device" "character varying(15)"                   Null
--              ]
--            , tableConstraints =
--              [ PrimaryKey "id" ]
--            }
--    sessionFact =
--      Fact { factName = "session"
--           , factTableName = "session_events"
--           , factParentNames = []
--           , factColumns =
--             [ DimTime "created_at"
--             , NoDimId "publisher_id"
--             , DimVal "user_agent" "browser_name"
--             , DimVal "user_agent" "os"
--             , DimVal "user_agent" "user_agent_name"
--             , DimVal "user_agent" "user_agent_type"
--             , DimVal "user_agent" "user_agent_device"
--             , DimVal "geo" "geo_country_name"
--             , DimVal "geo" "geo_city_name"
--             , DimVal "geo" "geo_continent_name"
--             , DimVal "geo" "geo_most_specific_subdivision_name"
--             , DimVal "geo" "geo_time_zone"
--             , FactCount Nothing "session_count"
--             ]
--           }
--    tables = [sessionEventsTable]
--    facts = [sessionFact]
--    typeDefaults = Map.fromList [ ("integer", "-1")
--                                , ("timestamp", "'00-00-00 00:00:00'")
--                                , ("character", "'__UNKNOWN_VAL__'")
--                                , ("uuid", "'00000000-0000-0000-0000-000000000000'::uuid")
--                                , ("boolean", "false")
--                                , ("json", "'{}'::json")
--                                , ("numeric", "-1")
--                                , ("text", "'__UNKNOWN_VAL__'")
--                                ]
--    settings = defSettings { settingTableNameSuffixTemplate = "" }
--    env = Env tables facts settings typeDefaults
-- :}

-- |
--
-- >>> print $ extractFactTable env sessionFact
-- Table fact_session_by_minute
-- Column created_at_minute_id bigint NOT NULL
-- Column publisher_id integer NOT NULL
-- Column session_count integer NOT NULL
-- Column geo_id integer NOT NULL
-- Column user_agent_id integer NOT NULL
-- UniqueKey (created_at_minute_id, publisher_id, geo_id, user_agent_id)
-- <BLANKLINE>
extractFactTable :: Env -> Fact -> Table
extractFactTable env = flip runReader env . E.extractFactTable

-- |
--
-- >>> mapM_ print $ extractDimensionTables env sessionFact
-- Table dim_geo
-- Column id serial NOT NULL
-- Column country_name character varying(50) NOT NULL
-- Column city_name character varying(50) NOT NULL
-- Column continent_name character varying(15) NOT NULL
-- Column most_specific_subdivision_name character varying(100) NOT NULL
-- Column time_zone character varying(20) NOT NULL
-- PrimaryKey id
-- UniqueKey (country_name, city_name, continent_name, most_specific_subdivision_name, time_zone)
-- <BLANKLINE>
-- Table dim_user_agent
-- Column id serial NOT NULL
-- Column browser_name character varying(50) NOT NULL
-- Column os character varying(50) NOT NULL
-- Column name character varying(100) NOT NULL
-- Column type character varying(15) NOT NULL
-- Column device character varying(15) NOT NULL
-- PrimaryKey id
-- UniqueKey (browser_name, os, name, type, device)
-- <BLANKLINE>
extractDimensionTables :: Env -> Fact -> [Table]
extractDimensionTables env = flip runReader env . E.extractDimensionTables

-- |
--
-- >>> putStrLn . ppShow $ extractDependencies env sessionFact
-- fromList
--   [ ( "dim_geo" , [ "session_events" ] )
--   , ( "dim_user_agent" , [ "session_events" ] )
--   , ( "fact_session_by_minute"
--     , [ "session_events" , "dim_user_agent" , "dim_geo" ]
--     )
--   ]
extractDependencies :: Env -> Fact -> Dependencies
extractDependencies env = flip runReader env . E.extractDependencies

-- |
--
-- >>> let storySessionDimTables = extractDimensionTables env sessionFact
-- >>> let sqls = map (tableDefnSQL env) storySessionDimTables
-- >>> mapM_ (\sqls -> mapM_ (putStr . Text.unpack) sqls >> putStrLn "--------" ) sqls
-- create table dim_geo (
--   id serial not null,
--   country_name character varying(50) not null,
--   city_name character varying(50) not null,
--   continent_name character varying(15) not null,
--   most_specific_subdivision_name character varying(100) not null,
--   time_zone character varying(20) not null
-- )
-- ;
-- <BLANKLINE>
-- alter table dim_geo add primary key (id);
-- <BLANKLINE>
-- alter table dim_geo add unique (country_name,
--                                 city_name,
--                                 continent_name,
--                                 most_specific_subdivision_name,
--                                 time_zone);
-- <BLANKLINE>
-- --------
-- create table dim_user_agent (
--   id serial not null,
--   browser_name character varying(50) not null,
--   os character varying(50) not null,
--   name character varying(100) not null,
--   type character varying(15) not null,
--   device character varying(15) not null
-- )
-- ;
-- <BLANKLINE>
-- alter table dim_user_agent add primary key (id);
-- <BLANKLINE>
-- alter table dim_user_agent add unique (browser_name,
--                                        os,
--                                        name,
--                                        type,
--                                        device);
-- <BLANKLINE>
-- --------
tableDefnSQL :: Env -> Table -> [Text]
tableDefnSQL env = flip runReader env . G.tableDefnSQL

-- |
--
-- >>> let storySessionFactTable = extractFactTable env sessionFact
-- >>> let sqls = factTableDefnSQL env sessionFact storySessionFactTable
-- >>> mapM_ (putStr . Text.unpack) sqls
-- create table fact_session_by_minute (
--   created_at_minute_id bigint not null,
--   publisher_id integer not null,
--   session_count integer not null,
--   geo_id integer not null,
--   user_agent_id integer not null
-- )
-- ;
-- <BLANKLINE>
-- alter table fact_session_by_minute add unique (created_at_minute_id,
--                                                publisher_id,
--                                                geo_id,
--                                                user_agent_id);
-- <BLANKLINE>
-- create index  on fact_session_by_minute (created_at_minute_id)
-- ;
-- create index  on fact_session_by_minute (publisher_id)
-- ;
-- create index  on fact_session_by_minute (geo_id)
-- ;
-- create index  on fact_session_by_minute (user_agent_id)
-- ;
factTableDefnSQL :: Env -> Fact -> Table -> [Text]
factTableDefnSQL env fact = flip runReader env . G.factTableDefnSQL fact

dimensionTablePopulateSQL :: TablePopulationMode -> Env -> Fact -> TableName -> Text
dimensionTablePopulateSQL popMode env fact =
  flip runReader env . G.dimensionTablePopulateSQL popMode fact

factTablePopulateSQL :: TablePopulationMode -> Env -> Fact -> [Text]
factTablePopulateSQL popMode env =
  flip runReader env . G.factTablePopulateSQL popMode

validateTable :: Env -> Table -> [ValidationError]
validateTable env = flip runReader env . V.validateTable

validateFact :: Env -> Fact -> [ValidationError]
validateFact env = flip runReader env . V.validateFact
