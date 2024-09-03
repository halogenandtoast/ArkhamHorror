module Arkham.Story.Cards.TimelessBeauty (TimelessBeauty (..), timelessBeauty) where

import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TimelessBeauty = TimelessBeauty StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timelessBeauty :: StoryCard TimelessBeauty
timelessBeauty = story TimelessBeauty Cards.timelessBeauty

instance RunMessage TimelessBeauty where
  runMessage msg s@(TimelessBeauty attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      iids <- select $ InvestigatorAt (locationIs Locations.serannian)
      for_ iids \iid -> do
        mResources <- gainResourcesIfCan iid attrs 2
        canHealDamage <- canHaveDamageHealed attrs iid
        for_ mResources push
        pushWhen canHealDamage $ HealDamage (toTarget iid) (toSource attrs) 2
      pure s
    _ -> TimelessBeauty <$> runMessage msg attrs
