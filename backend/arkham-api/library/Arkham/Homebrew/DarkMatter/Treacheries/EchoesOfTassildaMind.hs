module Arkham.Homebrew.DarkMatter.Treacheries.EchoesOfTassildaMind (
  echoesOfTassildaMind,
) where

import Arkham.Ability
import Arkham.Classes.HasGame
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted
import Arkham.Window (windowType)
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
mayBeSaved a victim = case a.inThreatAreaOf of
  Nothing -> pure True
  Just bearer
    | victim /= bearer -> pure True
    | otherwise -> (<= 1) <$> selectCount UneliminatedInvestigator

instance HasAbilities EchoesOfTassildaMind where
  getAbilities (EchoesOfTassildaMind a) =
    [mkAbility a 1 $ freeReaction $ InvestigatorWouldBeDefeated #when ByHorror Anyone]

instance RunMessage EchoesOfTassildaMind where
  runMessage msg t@(EchoesOfTassildaMind attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.WouldTakeHorror _ (InvestigatorTarget iid') n ->
          whenM (mayBeSaved attrs iid') do
            push $ CancelHorror iid' n
            addToVictory iid attrs
        _ -> pure ()
      pure t
    _ -> EchoesOfTassildaMind <$> liftRunMessage msg attrs
