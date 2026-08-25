module Arkham.Homebrew.DarkMatter.Treacheries.EchoesOfTassildaMind (
  echoesOfTassildaMind,
) where

import Arkham.Ability
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (replaceMessageMatching)
import Arkham.Helpers.Message (checkDefeated)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorAssignedHorror))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Import.Lifted hiding (checkDefeated)
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

{- | "Surge. Peril. Hidden. / Revelation - Secretly add this card to your hand. /
[reaction] When an investigator would be defeated by horror: Cancel that horror
and add this card to the victory display. (If you are not the only undefeated
investigator remaining, that investigator cannot be you.)"
-}
newtype EchoesOfTassildaMind = EchoesOfTassildaMind TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

echoesOfTassildaMind :: TreacheryCard EchoesOfTassildaMind
echoesOfTassildaMind = treachery EchoesOfTassildaMind Cards.echoesOfTassildaMind

{- | The parenthetical — "if you are not the only undefeated investigator
remaining, that investigator cannot be you" — has no matcher form (there is no
"only investigator" matcher), so the window admits anyone and the restriction is
enforced when the ability resolves.
-}
mayBeSaved :: HasGame m => TreacheryAttrs -> InvestigatorId -> m Bool
mayBeSaved a victim = case a.placement of
  HiddenInHand holder | holder == victim -> (<= 1) <$> selectCount UneliminatedInvestigator
  _ -> pure True

wouldBeDefeated :: [Window] -> Maybe InvestigatorId
wouldBeDefeated ws =
  listToMaybe [victim | (windowType -> Window.InvestigatorWouldBeDefeated _ victim) <- ws]

instance HasAbilities EchoesOfTassildaMind where
  getAbilities (EchoesOfTassildaMind a) =
    [restricted a 1 InYourHand $ freeReaction $ InvestigatorWouldBeDefeated #when ByHorror Anyone]

instance RunMessage EchoesOfTassildaMind where
  runMessage msg t@(EchoesOfTassildaMind attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (wouldBeDefeated -> Just victim) _ -> do
      -- the horror is already assigned by the time the window opens, so it is
      -- unwound with CancelAssignedDamage; the queued defeat becomes a
      -- CheckDefeated so anything else still defeating them is respected
      whenM (mayBeSaved attrs victim) do
        n <- field InvestigatorAssignedHorror victim
        lift
          $ replaceMessageMatching
            \case
              InvestigatorWhenDefeated _ victim' -> victim == victim'
              _ -> False
            \case
              InvestigatorWhenDefeated source _ -> [checkDefeated source victim]
              _ -> error "invalid match"
        push $ CancelAssignedDamage (toTarget victim) 0 n
        addToVictory iid attrs
      pure t
    _ -> EchoesOfTassildaMind <$> liftRunMessage msg attrs
