module Arkham.Act.Cards.ChildrenOfBlood.RiverOfBlood.LocateTheLair (locateTheLair) where

import Arkham.Ability
import Arkham.Act.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Lair))

newtype LocateTheLair = LocateTheLair ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

locateTheLair :: ActCard LocateTheLair
locateTheLair = act (1, A) LocateTheLair Cards.locateTheLair Nothing

instance HasAbilities LocateTheLair where
  getAbilities (LocateTheLair a) =
    [ restricted
        a
        1
        (exists $ YourLocation <> LocationWithTrait Lair <> LocationWithCardsUnderneath AnyCards)
        $ actionAbilityWithCost
        $ SameLocationGroupClueCost (Static 2) YourLocation
    , mkAbility a 2 $ Objective $ forced $ EnemyEntersPlay #when $ EnemyWithTitle "Julia Stern"
    ]

instance RunMessage LocateTheLair where
  runMessage msg a@(LocateTheLair attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        cards <- field LocationCardsUnderneath lid
        for_ cards $ drawCard iid
        when (notNull $ filterCards (CardWithTitle "Julia Stern") cards) $ scenarioSpecific_ "placeSnare"
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- if the agenda is mid-advance, it must finish resolving before we advance
      let advance = AdvanceAct attrs.id (toSource attrs) #other
      inserted <- insertAfterMatchingMaybe [advance] \case
        AdvanceAgendaDeck {} -> True
        _ -> False
      unless inserted $ push advance
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (#revealed <> not_ LocationWithVictory <> LocationNotAtClueLimit)
        $ placeCluesUpToClueValue attrs
      advanceActDeck attrs
      pure a
    _ -> LocateTheLair <$> liftRunMessage msg attrs
