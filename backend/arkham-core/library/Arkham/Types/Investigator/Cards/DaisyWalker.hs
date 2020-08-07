{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.DaisyWalker where

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson
import qualified Data.HashSet as HashSet

newtype DaisyWalkerMetadata = DaisyWalkerMetadata { tomeActions :: Int }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON) -- must parse to object

newtype DaisyWalker = DaisyWalker (Attrs `With` DaisyWalkerMetadata)
  deriving newtype (Show, ToJSON, FromJSON)

daisyWalker :: DaisyWalker
daisyWalker =
  DaisyWalker
    $ baseAttrs "01002" "Daisy Walker" stats [Miskatonic]
    `with` DaisyWalkerMetadata 1
 where
  stats = Stats
    { health = 5
    , sanity = 9
    , willpower = 3
    , intellect = 5
    , combat = 2
    , agility = 2
    }

becomesFailure :: Token -> Modifier -> Bool
becomesFailure token (ForcedTokenChange fromToken AutoFail _) =
  token == fromToken
becomesFailure _ _ = False

instance (InvestigatorRunner env) => RunMessage env DaisyWalker where
  runMessage msg i@(DaisyWalker (attrs@Attrs {..} `With` metadata@DaisyWalkerMetadata {..}))
    = case msg of
      ActivateCardAbilityAction iid (AssetSource aid, msource, abilityIndex, abilityType, abilityLimit)
        | iid == investigatorId
        -> do
          traits <- asks (getSet aid)
          if Tome `elem` traits && tomeActions > 0
            then case abilityType of
              FreeAbility _ ->
                DaisyWalker . (`with` metadata) <$> runMessage msg attrs
              ReactionAbility _ ->
                DaisyWalker . (`with` metadata) <$> runMessage msg attrs
              ActionAbility n actionType -> if n > 0
                then
                  DaisyWalker
                  . (`with` DaisyWalkerMetadata (tomeActions - 1))
                  <$> runMessage
                        (ActivateCardAbilityAction
                          iid
                          ( AssetSource aid
                          , msource
                          , abilityIndex
                          , ActionAbility (n - 1) actionType
                          , abilityLimit
                          )
                        )
                        attrs
                else DaisyWalker . (`with` metadata) <$> runMessage msg attrs
            else DaisyWalker . (`with` metadata) <$> runMessage msg attrs
      PlayerWindow iid additionalActions | iid == investigatorId ->
        if investigatorRemainingActions == 0 && tomeActions > 0
          then do
            let assetIds = HashSet.toList investigatorAssets
            tomeAssets <-
              map fst
              . filter ((Tome `elem`) . snd)
              . zip assetIds
              <$> traverse (asks . getSet) assetIds
            tomeAbilities <- mconcat <$> traverse (asks . getList) tomeAssets
            DaisyWalker
              . (`with` metadata)
              <$> runMessage
                    (PlayerWindow
                      iid
                      (additionalActions
                      <> [ ActivateCardAbilityAction iid ability
                         | ability <- tomeAbilities
                         ]
                      )
                    )
                    attrs
          else DaisyWalker . (`with` metadata) <$> runMessage msg attrs
      ResolveToken ElderSign iid skillValue | iid == investigatorId ->
        if any (becomesFailure ElderSign) investigatorModifiers
          then i <$ unshiftMessage (ResolveToken AutoFail iid skillValue)
          else do
            tomeCount <- unAssetCount <$> asks (getCount (iid, [Tome]))
            runTest skillValue -- Because this unshifts we need to call this before the on success is added
            when (tomeCount > 0) $ unshiftMessage
              (AddOnSuccess
                (Ask
                $ ChooseOne
                    [ DrawCards iid tomeCount False
                    , Continue "Do not use Daisy's ability"
                    ]
                )
              )
            pure i
      BeginRound ->
        DaisyWalker . (`with` DaisyWalkerMetadata 1) <$> runMessage msg attrs
      _ -> DaisyWalker . (`with` metadata) <$> runMessage msg attrs
