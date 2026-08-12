module Arkham.Homebrew.CircusExMortis.Treacheries.PrimordialEvils (primordialEvils) where

import Arkham.ChaosToken.Types
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (campaignI18n)
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Import.Lifted

newtype PrimordialEvils = PrimordialEvils TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primordialEvils :: TreacheryCard PrimordialEvils
primordialEvils = treachery PrimordialEvils Cards.primordialEvils

instance HasModifiersFor PrimordialEvils where
  getModifiersFor (PrimordialEvils a) = case a.placement of
    NextToAgenda ->
      modifySelect
        a
        (mapOneOf ChaosTokenFaceIs [Skull, Cultist, Tablet, ElderThing, MoonToken])
        [ChaosTokenValueModifier (-1)]
    _ -> pure mempty

instance RunMessage PrimordialEvils where
  runMessage msg t@(PrimordialEvils attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ campaignI18n $ scope "primordialEvils" do
        labeled' "reduceTokens" $ place attrs NextToAgenda
        labeled' "placeDoom" $ placeDoomOnAgendaAndCheckAdvanceBy attrs 1
      pure t
    _ -> PrimordialEvils <$> liftRunMessage msg attrs
