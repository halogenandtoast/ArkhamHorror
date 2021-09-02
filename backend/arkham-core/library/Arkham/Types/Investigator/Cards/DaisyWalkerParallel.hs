module Arkham.Types.Investigator.Cards.DaisyWalkerParallel
  ( DaisyWalkerParallel(..)
  , daisyWalkerParallel
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.InvestigatorId
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait

newtype DaisyWalkerParallel = DaisyWalkerParallel InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, ToJSON, FromJSON, Entity)

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
  getModifiersFor _ (InvestigatorTarget iid) (DaisyWalkerParallel attrs@InvestigatorAttrs {..})
    | iid == investigatorId
    = do
      tomeCount <- unAssetCount <$> getCount (investigatorId, [Tome])
      pure
        $ toModifiers attrs
        $ SkillModifier SkillWillpower tomeCount
        : [SanityModifier tomeCount]
  getModifiersFor _ _ _ = pure []

instance HasTokenValue env DaisyWalkerParallel where
  getTokenValue (DaisyWalkerParallel attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance HasAbilities DaisyWalkerParallel where
  getAbilities (DaisyWalkerParallel attrs) =
    [ restrictedAbility
          attrs
          1
          (Self <> AssetExists (AssetOwnedBy You <> AssetWithTrait Tome))
          (FastAbility Free)
        & (abilityLimitL .~ PlayerLimit PerGame 1)
    ]

instance InvestigatorRunner env => RunMessage env DaisyWalkerParallel where
  runMessage msg i@(DaisyWalkerParallel attrs@InvestigatorAttrs {..}) =
    case msg of
      UseCardAbility iid (InvestigatorSource iid') windows' 1 _
        | investigatorId == iid' -> do
          tomeAssets <- filterM
            ((elem Tome <$>) . getSet)
            (setToList investigatorAssets)
          let
            pairs' = filter (notNull . snd)
              $ map (\a -> (a, getAbilities a)) tomeAssets
          if null pairs'
            then pure i
            else i <$ push
              (chooseOneAtATime iid $ map
                (\(tome, actions) -> TargetLabel
                  (AssetTarget tome)
                  [ Run
                      [ chooseOne iid
                          $ map (($ windows') . UseAbility iid) actions
                      ]
                  ]
                )
                pairs'
              )
      UseCardAbility iid (TokenEffectSource ElderSign) _ 2 _
        | iid == investigatorId -> i
        <$ push (SearchDiscard iid (InvestigatorTarget iid) [Tome])
      ResolveToken _drawnToken ElderSign iid | iid == investigatorId ->
        i <$ push
          (chooseOne
            iid
            [ UseCardAbility iid (TokenEffectSource ElderSign) [] 2 NoPayment
            , Continue "Do not use Daisy's ability"
            ]
          )
      _ -> DaisyWalkerParallel <$> runMessage msg attrs
