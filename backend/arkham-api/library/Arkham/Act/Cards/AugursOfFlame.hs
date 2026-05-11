module Arkham.Act.Cards.AugursOfFlame (augursOfFlame) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Elite))

newtype AugursOfFlame = AugursOfFlame ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

augursOfFlame :: ActCard AugursOfFlame
augursOfFlame = act (1, A) AugursOfFlame Cards.augursOfFlame Nothing

instance HasAbilities AugursOfFlame where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ You <> at_ (LocationWithCardsUnderneath AnyCards))
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 2) Anywhere)
    , restricted
        a
        2
        ( ExtendedCardCount (atLeast 6)
            $ ExtendedCardWithOneOf
              [ VictoryDisplayCardMatch $ basic (#enemy <> withTrait Elite)
              , ExtendedCardMatches
                  [ CardIsBeneathActDeck
                  , basic (#enemy <> withTrait Elite)
                  ]
              ]
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage AugursOfFlame where
  runMessage msg a@(AugursOfFlame attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mLoc <- field InvestigatorLocation iid
      for_ mLoc \loc -> do
        cards <- field LocationCardsUnderneath loc
        for_ (listToMaybe cards) \card -> drawCard iid card
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      advanceActDeck attrs
      pure a
    _ -> AugursOfFlame <$> liftRunMessage msg attrs
