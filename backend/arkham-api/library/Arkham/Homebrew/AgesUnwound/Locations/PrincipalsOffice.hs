module Arkham.Homebrew.AgesUnwound.Locations.PrincipalsOffice (principalsOffice) where

import Arkham.Ability
import Arkham.GameEnv (getHistory)
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.History (History (historyActionsCompleted))
import Arkham.History.Types
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype PrincipalsOffice = PrincipalsOffice LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Mounting Dread/.
principalsOffice :: LocationCard PrincipalsOffice
principalsOffice = symbolLabel $ location PrincipalsOffice Cards.principalsOffice 1 (PerPlayer 2)

{- | "Principal's Office gets +1 shroud for each action you have completed this
turn."

The printed "you" has no window of its own, so -- as on /Yard/, whose shroud is
likewise read off an investigator -- it resolves to whoever is testing against
the location. Outside a skill test there is no "you" and the shroud is printed.
-}
instance HasModifiersFor PrincipalsOffice where
  getModifiersFor (PrincipalsOffice a) = whenRevealed a $ maybeModifySelf a do
    iid <- MaybeT getSkillTestInvestigator
    n <- lift $ historyActionsCompleted <$> getHistory TurnHistory iid
    guard (n > 0)
    pure [ShroudModifier n]

{- | "Haunted - For each action you have completed this turn, take 1 horror or
choose and discard a card from your hand."
-}
instance HasAbilities PrincipalsOffice where
  getAbilities (PrincipalsOffice a) =
    extendRevealed1 a $ campaignI18n $ hauntedI "principalsOffice.haunted" a 1

instance RunMessage PrincipalsOffice where
  runMessage msg l@(PrincipalsOffice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- historyActionsCompleted <$> getHistory TurnHistory iid
      doStep n msg
      pure l
    DoStep n (UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      hasCards <- fieldP InvestigatorHand notNull iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) 1
        when hasCards $ labeled "discardFromHand" $ chooseAndDiscardCard iid (attrs.ability 1)
      doNextStep msg
      pure l
    _ -> PrincipalsOffice <$> liftRunMessage msg attrs
