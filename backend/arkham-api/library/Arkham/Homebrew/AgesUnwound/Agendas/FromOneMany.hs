module Arkham.Homebrew.AgesUnwound.Agendas.FromOneMany (fromOneMany) where

import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.GameEnv (findAllCards)
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Traits (pattern Myriad)
import Arkham.Keyword qualified as Keyword
import Arkham.Trait (toTraits)

newtype FromOneMany = FromOneMany AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fromOneMany :: AgendaCard FromOneMany
fromOneMany = agenda (3, A) FromOneMany Cards.fromOneMany (Static 5)

-- | "Each [[Myriad]] treachery gains surge."
instance HasModifiersFor FromOneMany where
  getModifiersFor (FromOneMany a) = do
    myriadTreacheries <-
      findAllCards \c -> c.kind == TreacheryType && Myriad `elem` toTraits c
    modifyEach a myriadTreacheries [AddKeyword Keyword.Surge]

instance RunMessage FromOneMany where
  runMessage msg a@(FromOneMany attrs) = runQueueT $ case msg of
    -- "Each remaining investigator is defeated and suffers 1 physical trauma."
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> FromOneMany <$> liftRunMessage msg attrs
