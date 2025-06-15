module Arkham.Location.Cards.DescentToYoth (descentToYoth) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window

newtype Metadata = Metadata {flipDoom :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DescentToYoth = DescentToYoth (LocationAttrs `With` Metadata)
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descentToYoth :: LocationCard DescentToYoth
descentToYoth =
  symbolLabel
    $ location
      (DescentToYoth . (`with` Metadata False))
      Cards.descentToYoth
      3
      (Static 0)

instance HasAbilities DescentToYoth where
  getAbilities (DescentToYoth (a `With` _)) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ PutLocationIntoPlay #after Anyone (be a)
      , restricted a 2 (thisExists a LocationWithAnyDoom)
          $ freeReaction
          $ SkillTestResult #when You (WhileInvestigating $ be a) #success
      ]

instance RunMessage DescentToYoth where
  runMessage msg l@(DescentToYoth (attrs `With` metadata)) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> do
        chooseOneM iid do
          labeled "Place 1 doom on Descent to Yoth" $ placeDoom (attrs.ability 1) attrs 1
          labeled "Draw the top 2 cards of the encounter deck" $ drawEncounterCards iid attrs 2
      pure l
    UseCardAbility _iid (isSource attrs -> True) 2 _ _ ->
      pure $ DescentToYoth $ attrs `with` Metadata True
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ | flipDoom metadata -> do
      let lid = toId attrs
      checkWindows [mkWhen (Window.SuccessfulInvestigation iid lid)]
      push $ FlipDoom (toTarget attrs) 1
      checkWindows [mkAfter (Window.SuccessfulInvestigation iid lid)]
      pure $ DescentToYoth $ attrs `with` Metadata False
    _ -> DescentToYoth . (`with` metadata) <$> liftRunMessage msg attrs
