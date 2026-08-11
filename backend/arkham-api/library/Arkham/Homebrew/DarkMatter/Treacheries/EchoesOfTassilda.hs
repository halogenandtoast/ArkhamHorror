module Arkham.Homebrew.DarkMatter.Treacheries.EchoesOfTassilda (
  echoesOfTassildaMatter,
  echoesOfTassildaMind,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

{- | Both Echoes of Tassilda cards are "Surge. Peril. Hidden. / Revelation -
Secretly add this card to your hand." and print one reaction:

* (Matter) "When an investigator would be defeated by damage: Cancel that damage
  and add this card to the victory display."
* (Mind) "…by horror: Cancel that horror…"

"(If you are not the only undefeated investigator remaining, that investigator
cannot be you.)"
-}
newtype EchoesOfTassilda = EchoesOfTassilda TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkEchoes :: CardDef -> TreacheryCard EchoesOfTassilda
mkEchoes = treachery EchoesOfTassilda

echoesOfTassildaMatter :: TreacheryCard EchoesOfTassilda
echoesOfTassildaMatter = mkEchoes Cards.echoesOfTassildaMatter

echoesOfTassildaMind :: TreacheryCard EchoesOfTassilda
echoesOfTassildaMind = mkEchoes Cards.echoesOfTassildaMind

isMatter :: TreacheryAttrs -> Bool
isMatter a = toCardCode (toCardDef a) == toCardCode (Cards.echoesOfTassildaMatter :: CardDef)

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

instance HasAbilities EchoesOfTassilda where
  getAbilities (EchoesOfTassilda a) =
    [ mkAbility a 1
        $ freeReaction
        $ if isMatter a
          then InvestigatorWouldBeDefeated #when ByDamage Anyone
          else InvestigatorWouldBeDefeated #when ByHorror Anyone
    ]

instance RunMessage EchoesOfTassilda where
  runMessage msg t@(EchoesOfTassilda attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.WouldTakeDamage _ (InvestigatorTarget iid') n _ ->
          whenM (mayBeSaved attrs iid') do
            push $ CancelDamage iid' n
            addToVictory iid attrs
        Window.WouldTakeHorror _ (InvestigatorTarget iid') n ->
          whenM (mayBeSaved attrs iid') do
            push $ CancelHorror iid' n
            addToVictory iid attrs
        _ -> pure ()
      pure t
    _ -> EchoesOfTassilda <$> liftRunMessage msg attrs
