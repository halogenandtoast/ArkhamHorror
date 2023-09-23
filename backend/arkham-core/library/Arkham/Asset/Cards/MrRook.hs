module Arkham.Asset.Cards.MrRook (
  mrRook,
  MrRook (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher

newtype Metadata = Metadata {chosenCards :: [Card]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MrRook = MrRook (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mrRook :: AssetCard MrRook
mrRook = ally (MrRook . (`with` Metadata [])) Cards.mrRook (2, 2)

instance HasAbilities MrRook where
  getAbilities (MrRook (a `With` _)) =
    [ restrictedAbility a 1 ControlsThis
        $ FastAbility
        $ ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Secret 1
    ]

instance RunMessage MrRook where
  runMessage msg a@(MrRook (attrs `With` meta)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let
        goSearch n =
          Search
            iid
            (toSource attrs)
            (InvestigatorTarget iid)
            [fromTopOfDeck n]
            AnyCard
            (DeferSearchedToTarget $ toTarget attrs)
      push
        $ chooseOne
          iid
          [ Label "Top 3" [goSearch 3]
          , Label "Top 6" [goSearch 6]
          , Label "Top 9" [goSearch 9]
          ]
      pure a
    SearchFound iid target@(isTarget attrs -> True) deck cards
      | notNull cards -> do
          let
            anyWeaknesses = any (`cardMatch` WeaknessCard) cards
            nextStep = SearchFound iid target deck
          pushAll
            [ FocusCards cards
            , chooseOne
                iid
                [ targetLabel (toCardId card)
                  $ HandleTargetChoice
                    iid
                    (toSource attrs)
                    (CardTarget card)
                  : [ if anyWeaknesses
                        then
                          DoStep
                            (if card `cardMatch` WeaknessCard then 1 else 2)
                            (nextStep $ deleteFirst card cards)
                        else DoStep 3 msg
                    ]
                | card <- cards
                ]
            , UnfocusCards
            ]
          pure a
    DoStep 1 msg'@(SearchFound iid (isTarget attrs -> True) _ cards) -> do
      -- already selected weakness so we can take anything
      pushAll
        [ chooseOne
            iid
            [ targetLabel
              (toCardId card)
              [ HandleTargetChoice
                  iid
                  (toSource attrs)
                  (CardTarget card)
              ]
            | card <- cards
            ]
        , DoStep 3 msg'
        ]
      pure a
    DoStep 2 msg'@(SearchFound iid (isTarget attrs -> True) _ cards) -> do
      -- must select weakness
      pushAll
        [ chooseOne
            iid
            [ targetLabel
              (toCardId card)
              [ HandleTargetChoice
                  iid
                  (toSource attrs)
                  (CardTarget card)
              ]
            | card <- cards
            , card `cardMatch` WeaknessCard
            ]
        , DoStep 3 msg'
        ]
      pure a
    DoStep 3 (SearchFound iid (isTarget attrs -> True) _ _) -> do
      push $ AddToHand iid (chosenCards meta)
      pure $ MrRook (attrs `with` Metadata [])
    HandleTargetChoice _ (isSource attrs -> True) (CardTarget card) -> do
      pure
        $ MrRook
        $ attrs
        `with` Metadata
          { chosenCards = card : chosenCards meta
          }
    _ -> MrRook . (`with` meta) <$> runMessage msg attrs
