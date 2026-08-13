module Arkham.Homebrew.DarkMatter.Treacheries.EchoesOfTassildaMatter (
  echoesOfTassildaMatter,
) where

import Arkham.Ability
import Arkham.Classes.HasGame
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

{- | "Surge. Peril. Hidden. / Revelation - Secretly add this card to your hand. /
[reaction] When an investigator would be defeated by damage: Cancel that damage
and add this card to the victory display. (If you are not the only undefeated
investigator remaining, that investigator cannot be you.)"
-}
newtype EchoesOfTassildaMatter = EchoesOfTassildaMatter TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

echoesOfTassildaMatter :: TreacheryCard EchoesOfTassildaMatter
echoesOfTassildaMatter = treachery EchoesOfTassildaMatter Cards.echoesOfTassildaMatter

{- | The parenthetical — "if you are not the only undefeated investigator
remaining, that investigator cannot be you" — has no matcher form (there is no
"only investigator" matcher), so the window admits anyone and the restriction is
enforced when the ability resolves.
-}
mayBeSaved :: HasGame m => TreacheryAttrs -> InvestigatorId -> m Bool
mayBeSaved a victim = case a.inThreatAreaOf of
  Nothing -> pure True
  Just bearer
    | victim /= bearer -> pure True
    | otherwise -> (<= 1) <$> selectCount UneliminatedInvestigator

instance HasAbilities EchoesOfTassildaMatter where
  getAbilities (EchoesOfTassildaMatter a) =
    [mkAbility a 1 $ freeReaction $ InvestigatorWouldBeDefeated #when ByDamage Anyone]

instance RunMessage EchoesOfTassildaMatter where
  runMessage msg t@(EchoesOfTassildaMatter attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.WouldTakeDamage _ (InvestigatorTarget iid') n _ ->
          whenM (mayBeSaved attrs iid') do
            push $ CancelDamage iid' n
            addToVictory iid attrs
        _ -> pure ()
      pure t
    _ -> EchoesOfTassildaMatter <$> liftRunMessage msg attrs
