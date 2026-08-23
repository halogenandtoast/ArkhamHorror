module Arkham.Act.Cards.ChildrenOfBlood.NewHorizons.ToNewHorizons (toNewHorizons) where

import Arkham.Ability
import Arkham.Act.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.Treachery.CardDefs.ChildrenOfBlood.NewHorizons qualified as Treacheries

newtype ToNewHorizons = ToNewHorizons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toNewHorizons :: ActCard ToNewHorizons
toNewHorizons = act (1, A) ToNewHorizons Cards.toNewHorizons Nothing

instance HasAbilities ToNewHorizons where
  getAbilities = actAbilities \x ->
    [ onlyOnce $ restricted x 1 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    , mkAbility x 2 $ Objective $ triggered (RoundEnds #when) $ GroupClueCost (PerPlayer 3) "Storage"
    ]

instance RunMessage ToNewHorizons where
  runMessage msg a@(ToNewHorizons attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #clues attrs (attrs.ability 2)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      traverse_ placeLocation =<< getSetAsideCardsMatching (CardWithTitle "Cavern Entrance")
      push $ RemoveAllCopiesOfCardFromGame lead (toCardCode Enemies.factoryWorker)
      shuffleSetAsideIntoEncounterDeck
        $ oneOf [CardFromEncounterSet Set.FlyingTerrors, cardIs Treacheries.echoingInDarkness]
      advanceActDeck attrs
      pure a
    _ -> ToNewHorizons <$> liftRunMessage msg attrs
