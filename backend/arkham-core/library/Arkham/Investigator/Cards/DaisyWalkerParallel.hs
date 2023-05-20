module Arkham.Investigator.Cards.DaisyWalkerParallel
  ( DaisyWalkerParallel(..)
  , daisyWalkerParallel
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types ( Field (..) )
import Arkham.Card.CardType
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Zone qualified as Zone

newtype DaisyWalkerParallel = DaisyWalkerParallel InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisyWalkerParallel :: InvestigatorCard DaisyWalkerParallel
daisyWalkerParallel = investigator
  DaisyWalkerParallel
  Cards.daisyWalkerParallel
  Stats
    { health = 5
    , sanity = 7
    , willpower = 1
    , intellect = 5
    , combat = 2
    , agility = 2
    }

instance HasModifiersFor DaisyWalkerParallel where
  getModifiersFor (InvestigatorTarget iid) (DaisyWalkerParallel attrs@InvestigatorAttrs {..})
    | iid == investigatorId
    = do
      tomeCount <-
        selectCount
        $ AssetControlledBy (InvestigatorWithId investigatorId)
        <> AssetWithTrait Tome
      pure
        $ toModifiers attrs
        $ SkillModifier SkillWillpower tomeCount
        : [SanityModifier tomeCount]
  getModifiersFor _ _ = pure []

instance HasTokenValue DaisyWalkerParallel where
  getTokenValue iid ElderSign (DaisyWalkerParallel attrs)
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance HasAbilities DaisyWalkerParallel where
  getAbilities (DaisyWalkerParallel attrs) =
    [ restrictedAbility
          attrs
          1
          (Self <> AssetExists (AssetControlledBy You <> AssetWithTrait Tome))
          (FastAbility Free)
        & (abilityLimitL .~ PlayerLimit PerGame 1)
    ]

instance RunMessage DaisyWalkerParallel where
  runMessage msg i@(DaisyWalkerParallel attrs@InvestigatorAttrs {..}) =
    case msg of
      UseCardAbility iid (InvestigatorSource iid') 1 windows' _
        | investigatorId == iid' -> do
          tomeAssets <- filterM
            (fieldMap AssetTraits (member Tome))
            (setToList investigatorAssets)
          allAbilities <- getAllAbilities
          let
            abilitiesForAsset aid =
              filter ((AssetSource aid ==) . abilitySource) allAbilities
            pairs' = filter (notNull . snd)
              $ map (\a -> (a, abilitiesForAsset a)) tomeAssets
          unless (null pairs') $ push $ chooseOneAtATime iid $ map
            (\(tome, actions) -> TargetLabel
              (AssetTarget tome)
              [ chooseOne iid
                  $ map ((\f -> f windows' []) . AbilityLabel iid) actions
              ]
            )
            pairs'
          pure i
      UseCardAbility iid (TokenEffectSource ElderSign) 2 _ _
        | iid == investigatorId -> i <$ push
          (Search
              iid
              (toSource attrs)
              (toTarget attrs)
              [(Zone.FromDiscard, PutBack)]
              (CardWithType AssetType <> CardWithTrait Tome)
          $ DrawFound iid 1
          )
      ResolveToken _drawnToken ElderSign iid | iid == investigatorId -> do
        push $ chooseOne
          iid
          [ targetLabel
            iid
            [UseCardAbility iid (TokenEffectSource ElderSign) 2 [] NoPayment]
          , Label "Do not use Daisy's ability" []
          ]
        pure i
      _ -> DaisyWalkerParallel <$> runMessage msg attrs
