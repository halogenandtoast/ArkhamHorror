{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.DaisyWalkerParallel
  ( DaisyWalkerParallel(..)
  , daisyWalkerParallel
  )
where

import Arkham.Import

import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
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

instance HasCount AssetCount env (InvestigatorId, [Trait]) => HasModifiersFor env DaisyWalkerParallel where
  getModifiersFor source target@(InvestigatorTarget iid) (DaisyWalkerParallel attrs@Attrs {..})
    | iid == investigatorId
    = do
      tomeCount <- unAssetCount <$> getCount (investigatorId, [Tome])
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

instance HasTokenValue env DaisyWalkerParallel where
  getTokenValue (DaisyWalkerParallel attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue (DaisyWalkerParallel attrs) iid token =
    getTokenValue attrs iid token

instance ActionRunner env => HasActions env DaisyWalkerParallel where
  getActions iid FastPlayerWindow (DaisyWalkerParallel attrs)
    | iid == investigatorId attrs = do
      baseActions <- getActions iid FastPlayerWindow attrs
      let ability' = (iid, ability attrs)
      unused <- notElem ability' . map unUsedAbility <$> getList ()
      hasTomes <- (> 0) . unAssetCount <$> getCount (iid, [Tome])
      pure
        $ [ uncurry ActivateCardAbilityAction ability' | unused && hasTomes ]
        <> baseActions
  getActions i window (DaisyWalkerParallel attrs) = getActions i window attrs

instance InvestigatorRunner env => RunMessage env DaisyWalkerParallel where
  runMessage msg i@(DaisyWalkerParallel attrs@Attrs {..}) = case msg of
    UseCardAbility iid (InvestigatorSource iid') _ 1 | investigatorId == iid' ->
      do
        tomeAssets <- filterM
          ((elem Tome <$>) . getSet)
          (setToList investigatorAssets)
        pairs' <-
          filter (not . null . snd)
            <$> traverse (\a -> (a, ) <$> getActions iid NonFast a) tomeAssets
        if null pairs'
          then pure i
          else i <$ unshiftMessage
            (Ask iid . ChooseOneAtATime $ map
              (\(tome, actions) ->
                TargetLabel (AssetTarget tome) [Run [chooseOne iid actions]]
              )
              pairs'
            )
    UseCardAbility iid (TokenEffectSource ElderSign) _ 2
      | iid == investigatorId
      -> i <$ unshiftMessage (SearchDiscard iid (InvestigatorTarget iid) [Tome])
    ResolveToken _drawnToken ElderSign iid | iid == investigatorId ->
      i <$ unshiftMessage
        (chooseOne
          iid
          [ UseCardAbility iid (TokenEffectSource ElderSign) Nothing 2
          , Continue "Do not use Daisy's ability"
          ]
        )
    _ -> DaisyWalkerParallel <$> runMessage msg attrs
