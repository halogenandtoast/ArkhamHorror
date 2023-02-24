module Arkham.Asset.Cards.OldBookOfLore3
  ( OldBookOfLore3(..)
  , oldBookOfLore3
  , oldBookOfLore3Effect
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Deck qualified as Deck
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window
import Arkham.Matcher

newtype OldBookOfLore3 = OldBookOfLore3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore3 :: AssetCard OldBookOfLore3
oldBookOfLore3 = asset OldBookOfLore3 Cards.oldBookOfLore3

instance HasAbilities OldBookOfLore3 where
  getAbilities (OldBookOfLore3 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis <> InvestigatorExists
            (InvestigatorAt YourLocation
            <> InvestigatorWithoutModifier CannotManipulateDeck
            )
          )
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, ExhaustCost $ toTarget a]
    ]

instance RunMessage OldBookOfLore3 where
  runMessage msg a@(OldBookOfLore3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      investigatorIds <- selectList $ colocatedWith iid
      push $ chooseOne
        iid
        [ targetLabel
            iid'
            [ Search
                iid'
                source
                (InvestigatorTarget iid')
                [fromTopOfDeck 3]
                AnyCard
                (DeferSearchedToTarget $ toTarget attrs)
            ]
        | iid' <- investigatorIds
        ]
      pure a
    SearchFound iid (isTarget attrs -> True) (Deck.InvestigatorDeck iid') targetCards -> do
      let windows' = [Window Timing.When (Window.DuringTurn iid)]
      mEndSearch <- popMessageMatching $ \case
        EndSearch{} -> True
        _ -> False
      case mEndSearch of
        Nothing -> error "no matching end search"
        Just endSearch -> do
          choices <- for targetCards $ \card -> do
            spendableResources <- getSpendableResources iid'
            playable <- getIsPlayableWithResources iid' (toSource attrs) (spendableResources + 2) UnpaidCost windows' card
            pure $ TargetLabel (CardIdTarget $ toCardId card) $ AddFocusedToHand iid (InvestigatorTarget iid') FromDeck (toCardId card)
              : endSearch
              : [ chooseOne iid
                  [ Label
                    "Spend secret"
                    [ SpendUses (toTarget attrs) Secret 1
                    , createCardEffect Cards.oldBookOfLore3 Nothing (toSource attrs) (CardIdTarget $ toCardId card)
                    , PayCardCost iid' card windows'
                    ]
                  , Label "Do not spend secret" []
                  ]
                | playable && useCount (assetUses attrs) > 0
                ]
          push $ chooseOne iid' choices
      pure a
    _ -> OldBookOfLore3 <$> runMessage msg attrs

newtype OldBookOfLore3Effect = OldBookOfLore3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore3Effect :: EffectArgs -> OldBookOfLore3Effect
oldBookOfLore3Effect = OldBookOfLore3Effect . uncurry4 (baseAttrs "03033")

instance HasModifiersFor OldBookOfLore3Effect where
  getModifiersFor target@(CardIdTarget cid) (OldBookOfLore3Effect attrs)
    | effectTarget attrs == target = pure
    $ toModifiers attrs [ReduceCostOf (CardWithId cid) 2]
  getModifiersFor _ _ = pure []

instance RunMessage OldBookOfLore3Effect where
  runMessage msg e@(OldBookOfLore3Effect attrs) = case msg of
    ResolvedCard _ card | CardIdTarget (toCardId card) == effectTarget attrs ->
      e <$ push (DisableEffect $ toId attrs)
    _ -> OldBookOfLore3Effect <$> runMessage msg attrs
