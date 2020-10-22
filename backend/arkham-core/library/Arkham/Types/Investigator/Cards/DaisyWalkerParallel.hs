{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.DaisyWalkerParallel
  ( DaisyWalkerParallel(..)
  , daisyWalkerParallel
  )
where

import Arkham.Import

import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait

newtype DaisyWalkerParallel = DaisyWalkerParallel Attrs
  deriving newtype (Show, ToJSON, FromJSON)

daisyWalkerParallel :: DaisyWalkerParallel
daisyWalkerParallel = DaisyWalkerParallel
  $ baseAttrs "90001" "Daisy Walker" Seeker stats [Miskatonic]
 where
  stats = Stats
    { health = 5
    , sanity = 7
    , willpower = 1
    , intellect = 5
    , combat = 2
    , agility = 2
    }

instance InvestigatorRunner env => HasModifiersFor env DaisyWalkerParallel where
  getModifiersFor source target@(InvestigatorTarget iid) (DaisyWalkerParallel attrs@Attrs {..})
    | iid == investigatorId
    = do
      tomeCount <- asks $ unAssetCount . getCount (investigatorId, [Tome])
      baseModifiers <- getModifiersFor source target attrs
      pure
        $ SkillModifier SkillWillpower tomeCount
        : SanityModifier tomeCount
        : baseModifiers
  getModifiersFor source target (DaisyWalkerParallel attrs) =
    getModifiersFor source target attrs

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (FastAbility FastPlayerWindow))
  { abilityLimit = PerGame
  }

instance ActionRunner env => HasActions env DaisyWalkerParallel where
  getActions iid FastPlayerWindow (DaisyWalkerParallel attrs)
    | iid == investigatorId attrs = do
      baseActions <- getActions iid FastPlayerWindow attrs
      let ability' = (iid, ability attrs)
      unused <- asks $ notElem ability' . map unUsedAbility . getList ()
      pure
        $ [ uncurry ActivateCardAbilityAction ability' | unused ]
        <> baseActions
  getActions i window (DaisyWalkerParallel attrs) = getActions i window attrs

becomesFailure :: Token -> Modifier -> Bool
becomesFailure token (ForcedTokenChange fromToken AutoFail) =
  token == fromToken
becomesFailure _ _ = False

instance InvestigatorRunner env => RunMessage env DaisyWalkerParallel where
  runMessage msg i@(DaisyWalkerParallel attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ _ 1 -> do
      tomeAssets <- filterM
        (asks . (elem Tome .) . getSet)
        (setToList investigatorAssets)
      pairs' <- traverse (getActions iid NonFast) tomeAssets
      i <$ unshiftMessage
        (Ask iid . ChooseOneAtATime . map (chooseOne iid) $ filter
          (not . null)
          pairs'
        )
    UseCardAbility iid (TokenSource ElderSign) _ 2 ->
      i <$ unshiftMessage (SearchDiscard iid (InvestigatorTarget iid) [Tome])
    ResolveToken ElderSign iid | iid == investigatorId ->
      if any
          (becomesFailure ElderSign)
          (concat . toList $ investigatorModifiers)
        then i <$ unshiftMessage (ResolveToken AutoFail iid)
        else do
          runTest investigatorId (TokenValue ElderSign 1)
          i <$ unshiftMessage
            (chooseOne
              iid
              [ UseCardAbility iid (TokenSource ElderSign) Nothing 2
              , Continue "Do not use Daisy's ability"
              ]
            )
    _ -> DaisyWalkerParallel <$> runMessage msg attrs
