module Arkham.Homebrew.AgesUnwound.Locations.Cairo (cairo) where

import Arkham.Ability
import Arkham.GameEnv (getHistory)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModifySelf)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.History (History (historyActionsCompleted))
import Arkham.History.Types
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype Cairo = Cairo LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cairo :: LocationCard Cairo
cairo = symbolLabel $ location Cairo Cards.cairo 4 (PerPlayer 1)

{- | "Cairo gets +1 shroud for each action you have completed this turn."

The printed "you" has no window of its own, so -- as on Principal's Office,
which prints the same sentence -- it resolves to whoever is testing against the
location. Outside a skill test there is no "you" and the shroud is printed.
-}
instance HasModifiersFor Cairo where
  getModifiersFor (Cairo a) = whenRevealed a $ maybeModifySelf a do
    iid <- MaybeT getSkillTestInvestigator
    n <- lift $ historyActionsCompleted <$> getHistory TurnHistory iid
    guard (n > 0)
    pure [ShroudModifier n]

{- | "[reaction] After you successfully investigate Cairo: Search the top 3 cards
of your deck for a card and draw it."
-}
instance HasAbilities Cairo where
  getAbilities (Cairo a) =
    extendRevealed1 a
      $ campaignI18n
      $ withI18nTooltip "cairo.search"
      $ restricted a 1 Here
      $ freeReaction (SuccessfulInvestigation #after You (be a))

instance RunMessage Cairo where
  runMessage msg l@(Cairo attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 3] #any (DrawFound iid 1)
      pure l
    _ -> Cairo <$> liftRunMessage msg attrs
