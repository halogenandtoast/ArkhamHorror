module Arkham.Treachery.Cards.ToFightTheBlackWind (
  toFightTheBlackWind,
  ToFightTheBlackWind (..),
)
where

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ToFightTheBlackWind = ToFightTheBlackWind TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toFightTheBlackWind :: TreacheryCard ToFightTheBlackWind
toFightTheBlackWind = treachery ToFightTheBlackWind Cards.toFightTheBlackWind

instance HasAbilities ToFightTheBlackWind where
  getAbilities (ToFightTheBlackWind x) =
    [ restrictedAbility x 1 (exists InvestigatorWithUnhealedHorror) $ forced $ RoundEnds #when
    ]

instance RunMessage ToFightTheBlackWind where
  runMessage msg t@(ToFightTheBlackWind attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      agendas <- select AnyAgenda
      when (notNull agendas) $ do
        chooseOrRunOne
          iid
          [targetLabel agenda [AttachTreachery (toId attrs) (toTarget agenda)] | agenda <- agendas]
      mCarolynFern <- selectOne $ investigatorIs Investigators.carolynFern

      for_ mCarolynFern \carolynFern -> assignHorror carolynFern attrs 1

      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoomOnAgenda 1
      pure t
    _ -> ToFightTheBlackWind <$> lift (runMessage msg attrs)
