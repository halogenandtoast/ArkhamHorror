module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.ParisSewers_021 (parisSewers_021) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Direction
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype ParisSewers_021 = ParisSewers_021 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parisSewers_021 :: LocationCard ParisSewers_021
parisSewers_021 =
  locationWith ParisSewers_021 Cards.parisSewers_021 3 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities ParisSewers_021 where
  getAbilities (ParisSewers_021 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "parisSewers_021.sacrifice"
      $ restricted a 1 (Here <> exists (ConcealedCardAt (be a)) <> youExist (HandWith $ HasCard AnyCard))
      $ freeReaction
      $ SkillTestResult #after You WhileEvading #failure

getFailedBy :: [Window] -> Int
getFailedBy = \case
  (windowType -> Window.FailSkillTest _ n) : _ -> n
  _ : rest -> getFailedBy rest
  [] -> 0

instance RunMessage ParisSewers_021 where
  runMessage msg l@(ParisSewers_021 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getFailedBy -> n) _ -> do
      cards <- select $ inHandOf NotForPlay iid <> basic (oneOf $ map CardWithCost [n .. 20])
      focusCards cards $ chooseTargetM iid cards \card -> do
        discardCard iid (attrs.ability 1) card
        doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      chooseRevealConcealedAt iid (attrs.ability 1) (be attrs)
      pure l
    _ -> ParisSewers_021 <$> liftRunMessage msg attrs
