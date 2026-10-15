module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.ParisSewers_020 (parisSewers_020) where

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

newtype ParisSewers_020 = ParisSewers_020 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parisSewers_020 :: LocationCard ParisSewers_020
parisSewers_020 =
  locationWith ParisSewers_020 Cards.parisSewers_020 3 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities ParisSewers_020 where
  getAbilities (ParisSewers_020 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "parisSewers_020.sacrifice"
      $ restricted a 1 (Here <> exists (ConcealedCardAt (be a)) <> youExist (HandWith $ HasCard AnyCard))
      $ freeReaction
      $ SkillTestResult #after You WhileAttacking #failure

getFailedBy :: [Window] -> Int
getFailedBy = \case
  (windowType -> Window.FailSkillTest _ n) : _ -> n
  _ : rest -> getFailedBy rest
  [] -> 0

instance RunMessage ParisSewers_020 where
  runMessage msg l@(ParisSewers_020 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getFailedBy -> n) _ -> do
      cards <- select $ inHandOf NotForPlay iid <> basic (oneOf $ map CardWithCost [n .. 20])
      focusCards cards $ chooseTargetM iid cards \card -> do
        discardCard iid (attrs.ability 1) card
        doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      chooseRevealConcealedAt iid (attrs.ability 1) (be attrs)
      pure l
    _ -> ParisSewers_020 <$> liftRunMessage msg attrs
