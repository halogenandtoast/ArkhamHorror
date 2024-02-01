module Arkham.Asset.Cards.Kleptomania (
  kleptomania,
  Kleptomania (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Deck qualified as Deck
import Arkham.Matcher

newtype Kleptomania = Kleptomania AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

kleptomania :: AssetCard Kleptomania
kleptomania = asset Kleptomania Cards.kleptomania

instance HasAbilities Kleptomania where
  getAbilities (Kleptomania a) =
    [ controlledAbility
        a
        1
        ( atYourLocation
            $ affectsOthers
            $ NotYou
            <> oneOf [InvestigatorWithResources (atLeast 2), HasMatchingAsset #item]
        )
        actionAbility
    , restrictedAbility a 2 ControlsThis $ ForcedAbility $ TurnEnds #when You
    ]

instance RunMessage Kleptomania where
  runMessage msg t@(Kleptomania attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ putCardIntoPlay iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- selectList $ AssetOwnedBy (notInvestigator iid <> colocatedWith iid) <> #item
      investigators <-
        selectList $ notInvestigator iid <> colocatedWith iid <> InvestigatorWithResources (atLeast 2)
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ targetLabel
            iid'
            [ LoseResources iid' (toAbilitySource attrs 1) 2
            , takeResources iid iid' 2
            , ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs)
            ]
          | iid' <- investigators
          ]
        <> [ targetLabel
            aid
            [TakeControlOfAsset iid aid, ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs)]
           | aid <- assets
           ]
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ assignHorror iid (toAbilitySource attrs 2) 1
      pure t
    _ -> Kleptomania <$> runMessage msg attrs
