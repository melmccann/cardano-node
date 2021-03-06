module Cardano.CLI.Byron.Parsers
  ( ByronCommand(..)
  , parseByronCommands
  , parseHeavyDelThd
  , parseInstallerHash
  , parseMaxBlockSize
  , parseMaxHeaderSize
  , parseMaxTxSize
  , parseMaxProposalSize
  , parseMpcThd
  , parseScriptVersion
  , parseSlotDuration
  , parseSoftforkRuleParam
  , parseSystemTag
  , parseTxFeePolicy
  , parseUpdateProposalThd
  , parseUpdateProposalTTL
  , parseUnlockStakeEpoch
  , parseUpdateVoteThd
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import qualified Data.ByteString.Lazy.Char8 as C8
import           Formatting (build, sformat)
import           Options.Applicative

import           Cardano.Chain.Common
                   (TxFeePolicy(..), TxSizeLinear(..), rationalToLovelacePortion)
import           Cardano.Crypto.Hashing (hashRaw)
import           Cardano.Chain.Slotting (EpochNumber(..), SlotNumber(..))
import           Cardano.Chain.Update
                   (ApplicationName(..), InstallerHash(..), NumSoftwareVersion,
                    ProtocolVersion(..), SoftforkRule(..), SoftwareVersion(..), SystemTag(..),
                    checkApplicationName, checkSystemTag)

import           Cardano.CLI.Byron.UpdateProposal
import           Cardano.Common.Parsers
                   (command', parseCLISocketPath, parseConfigFile, parseFilePath,
                    parseFraction, parseLovelace, parseSigningKeyFile)
import           Cardano.Config.Types

-- TODO: Other Byron commands to be put here in follow up PR.
data ByronCommand
  = CreateVote
        ConfigYamlFilePath
        SigningKeyFile
        FilePath -- filepath to update proposal
        Bool
        FilePath
  | UpdateProposal
        ConfigYamlFilePath
        SigningKeyFile
        ProtocolVersion
        SoftwareVersion
        SystemTag
        InstallerHash
        FilePath
        [ParametersToUpdate]
  | SubmitUpdateProposal
        ConfigYamlFilePath
        -- ^ Update proposal filepath.
        FilePath
        (Maybe CLISocketPath)
  | SubmitVote
        ConfigYamlFilePath
        FilePath
        -- ^ Vote filepath.
        (Maybe CLISocketPath)

  deriving Show

parseByronCommands :: Parser ByronCommand
parseByronCommands =  subparser $ mconcat
    [ commandGroup "Byron related commands"
    , metavar "Byron related commands"
    , command' "create-byron-update-proposal" "Create a Byron era update proposal."
        $ parseByronUpdateProposal
    , command' "create-byron-proposal-vote" "Create a Byron era proposal vote."
        $ parseByronVote
    , command' "submit-byron-update-proposal" "Submit a Byron era update proposal."
        $ parseByronUpdateProposalSubmission
    , command' "submit-byron-proposal-vote" "Submit a Byron era proposal vote."
        $ parseByronVoteSubmission
    ]

parseByronUpdateProposal :: Parser ByronCommand
parseByronUpdateProposal = do
  UpdateProposal
    <$> (ConfigYamlFilePath <$> parseConfigFile)
    <*> parseSigningKeyFile "signing-key" "Path to signing key."
    <*> parseProtocolVersion
    <*> parseSoftwareVersion
    <*> parseSystemTag
    <*> parseInstallerHash
    <*> parseFilePath "filepath" "Byron proposal output filepath."
    <*> parseParametersToUpdate

parseByronVoteSubmission :: Parser ByronCommand
parseByronVoteSubmission = do
  SubmitVote
    <$> (ConfigYamlFilePath <$> parseConfigFile)
    <*> parseFilePath "filepath" "Filepath of Byron update proposal vote."
    <*> parseCLISocketPath "Path to a cardano-node socket."

parseParametersToUpdate :: Parser [ParametersToUpdate]
parseParametersToUpdate =
  catMaybes
    <$> sequenceA
          [ parseScriptVersion
          , parseSlotDuration
          , parseMaxBlockSize
          , parseMaxHeaderSize
          , parseMaxTxSize
          , parseMaxProposalSize
          , parseMpcThd
          , parseHeavyDelThd
          , parseUpdateVoteThd
          , parseUpdateProposalThd
          , parseUpdateProposalTTL
          , parseSoftforkRuleParam
          , parseTxFeePolicy
          , parseUnlockStakeEpoch
          ]

parseByronUpdateProposalSubmission :: Parser ByronCommand
parseByronUpdateProposalSubmission =
  SubmitUpdateProposal
    <$> (ConfigYamlFilePath <$> parseConfigFile)
    <*> parseFilePath "filepath" "Filepath of Byron update proposal."
    <*> parseCLISocketPath "Path to a cardano-node socket."


parseByronVote :: Parser ByronCommand
parseByronVote =
  CreateVote
    <$> (ConfigYamlFilePath <$> parseConfigFile)
    <*> (SigningKeyFile <$> parseFilePath "signing-key" "Filepath of signing key.")
    <*> parseFilePath "proposal-filepath" "Filepath of Byron update proposal."
    <*> parseVoteBool
    <*> parseFilePath "output-filepath" "Byron vote output filepath."

--------------------------------------------------------------------------------
-- CLI Parsers
--------------------------------------------------------------------------------

parseScriptVersion :: Parser (Maybe ParametersToUpdate)
parseScriptVersion = optional $
  ScriptVersion <$> option auto
                      ( long "script-version"
                      <> metavar "WORD16"
                      <> help "Proposed script version."
                      )

parseSlotDuration :: Parser (Maybe ParametersToUpdate)
parseSlotDuration = optional $
  SlotDuration <$> option auto
                     ( long "slot-duration"
                     <> metavar "NATURAL"
                     <> help "Proposed slot duration."
                     )

parseSystemTag :: Parser SystemTag
parseSystemTag = option (eitherReader checkSysTag)
                   ( long "system-tag"
                   <> metavar "STRING"
                   <> help "Identify which system (linux, win64, etc) the update proposal is for."
                   )
 where
  checkSysTag :: String -> Either String SystemTag
  checkSysTag name =
    let tag = SystemTag $ toS name
    in case checkSystemTag tag of
         Left err -> Left . toS $ sformat build err
         Right () -> Right tag

parseInstallerHash :: Parser InstallerHash
parseInstallerHash =
  InstallerHash .  hashRaw . C8.pack
    <$> strOption ( long "installer-hash"
                  <> metavar "HASH"
                  <> help "Software hash."
                  )

parseMaxBlockSize :: Parser (Maybe ParametersToUpdate)
parseMaxBlockSize = optional $
  MaxBlockSize <$> option auto
                     ( long "max-block-size"
                     <> metavar "NATURAL"
                     <> help "Proposed max block size."
                     )

parseMaxHeaderSize :: Parser (Maybe ParametersToUpdate)
parseMaxHeaderSize = optional $
  MaxHeaderSize <$> option auto
                      ( long "max-header-size"
                      <> metavar "NATURAL"
                      <> help "Proposed max block header size."
                      )

parseMaxTxSize :: Parser (Maybe ParametersToUpdate)
parseMaxTxSize = optional $
  MaxTxSize <$> option auto
                  ( long "max-tx-size"
                  <> metavar "NATURAL"
                  <> help "Proposed max transaction size."
                  )

parseMaxProposalSize :: Parser (Maybe ParametersToUpdate)
parseMaxProposalSize = optional $
  MaxProposalSize <$> option auto
                        ( long "max-proposal-size"
                        <> metavar "NATURAL"
                        <> help "Proposed max update proposal size."
                        )

parseMpcThd :: Parser (Maybe ParametersToUpdate)
parseMpcThd = optional $
  MpcThd . rationalToLovelacePortion
    <$> parseFraction "max-mpc-thd" "Proposed max mpc threshold."

parseProtocolVersion :: Parser ProtocolVersion
parseProtocolVersion =
  ProtocolVersion <$> (parseWord "protocol-version-major" "Protocol verson major." "WORD16" :: Parser Word16)
                  <*> (parseWord "protocol-version-minor" "Protocol verson minor." "WORD16" :: Parser Word16)
                  <*> (parseWord "protocol-version-alt" "Protocol verson alt." "WORD8" :: Parser Word8)

parseHeavyDelThd :: Parser (Maybe ParametersToUpdate)
parseHeavyDelThd = optional $
  HeavyDelThd . rationalToLovelacePortion
    <$> parseFraction "heavy-del-thd" "Proposed heavy delegation threshold."

parseUpdateVoteThd :: Parser (Maybe ParametersToUpdate)
parseUpdateVoteThd = optional $
  UpdateVoteThd . rationalToLovelacePortion
    <$> parseFraction "update-vote-thd" "Propose update vote threshold."

parseUpdateProposalThd :: Parser (Maybe ParametersToUpdate)
parseUpdateProposalThd = optional $
  UpdateProposalThd . rationalToLovelacePortion
    <$> parseFraction "update-proposal-thd" "Propose update proposal threshold."

parseUpdateProposalTTL :: Parser (Maybe ParametersToUpdate)
parseUpdateProposalTTL = optional $
  UpdateProposalTTL . SlotNumber
    <$> option auto
          ( long "time-to-live"
          <> metavar "WORD64"
          <> help "Proposed time for an update proposal to live."
          )

parseSoftforkRuleParam :: Parser (Maybe ParametersToUpdate)
parseSoftforkRuleParam = optional $
  SoftforkRuleParam
    <$> (SoftforkRule
           <$> (rationalToLovelacePortion <$> parseFraction "softfork-init-thd" "Propose initial threshold (right after proposal is confirmed).")
           <*> (rationalToLovelacePortion <$> parseFraction "softfork-min-thd" "Propose minimum threshold (threshold can't be less than this).")
           <*> (rationalToLovelacePortion <$> parseFraction "softfork-thd-dec" "Propose threshold decrement (threshold will decrease by this amount after each epoch).")
        )

parseSoftwareVersion :: Parser SoftwareVersion
parseSoftwareVersion =
  SoftwareVersion <$> parseApplicationName <*> parseNumSoftwareVersion

parseApplicationName :: Parser ApplicationName
parseApplicationName = option (eitherReader checkAppNameLength)
       (  long "application-name"
       <> metavar "STRING"
       <> help "The name of the application."
       )
 where
  checkAppNameLength :: String -> Either String ApplicationName
  checkAppNameLength name =
    let appName = ApplicationName $ toS name
    in case checkApplicationName appName of
         Left err -> Left . toS $ sformat build err
         Right () -> Right appName

parseNumSoftwareVersion :: Parser NumSoftwareVersion
parseNumSoftwareVersion =
  parseWord
    "software-version-num"
    "Numeric software version associated with application name."
    "WORD32"

parseTxFeePolicy :: Parser (Maybe ParametersToUpdate)
parseTxFeePolicy = optional $
  TxFeePolicy . TxFeePolicyTxSizeLinear
    <$> ( TxSizeLinear <$> parseLovelace "tx-fee-a-constant" "Propose the constant a for txfee = a + b*s where s is the size."
                       <*> parseFraction "tx-fee-b-constant" "Propose the constant b for txfee = a + b*s where s is the size."
        )

parseVoteBool :: Parser Bool
parseVoteBool = flag' True (long "vote-yes" <> help "Vote yes with respect to an update proposal.")
            <|> flag' False (long "vote-no" <> help "Vote no with respect to an update proposal.")

parseUnlockStakeEpoch :: Parser (Maybe ParametersToUpdate)
parseUnlockStakeEpoch = optional $
  UnlockStakeEpoch . EpochNumber
    <$> option auto
      ( long "unlock-stake-epoch"
      <> metavar "WORD64"
      <> help "Proposed epoch to unlock all stake."
      )


parseWord :: Integral a => String -> String -> String -> Parser a
parseWord optname desc metvar = option (fromInteger <$> auto)
  $ long optname <> metavar metvar <> help desc
