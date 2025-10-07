module Arkham.Treachery.Cards.FiguresInTheDark (figuresInTheDark) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FiguresInTheDark = FiguresInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

figuresInTheDark :: TreacheryCard FiguresInTheDark
figuresInTheDark = treachery FiguresInTheDark Cards.figuresInTheDark

instance RunMessage FiguresInTheDark where
  runMessage msg t@(FiguresInTheDark attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      inShadow <- select $ EnemyWithPlacement InTheShadows
      if null inShadow
        then gainSurge attrs
        else chooseOneM iid $ campaignI18n do
          labeled' "figuresInTheDark.attack" do
            chooseOneAtATimeM iid do
              targets inShadow \enemy -> initiateEnemyAttack enemy attrs iid
          labeled' "figuresInTheDark.loseAction" do
            eachInvestigator \iid' -> loseActions iid' attrs 1
      pure t
    _ -> FiguresInTheDark <$> liftRunMessage msg attrs
