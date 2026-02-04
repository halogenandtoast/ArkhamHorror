module Arkham.Act.Cards.WheresBertie (wheresBertie) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardDef
import Arkham.ForMovement
import Arkham.Helpers.Query (getInvestigators)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype WheresBertie = WheresBertie ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wheresBertie :: ActCard WheresBertie
wheresBertie = act (2, A) WheresBertie Cards.wheresBertie Nothing

instance HasAbilities WheresBertie where
  getAbilities (WheresBertie a) =
    [ restricted a 1 (exists $ ConnectedFrom NotForMovement YourLocation <> UnrevealedLocation)
        $ FastAbility (ClueCost $ Static 1)
    ]

instance RunMessage WheresBertie where
  runMessage msg a@(WheresBertie attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      bertie <- fetchCard Assets.bertieMusgraveATrueAesthete
      let def = toCardDef bertie
      investigators <- getInvestigators
      leadChooseOneM do
        withI18n
          $ nameVar def
          $ questionLabeled' "chooseInvestigatorToTakeControlOf"
        questionLabeledCard def
        portraits investigators (`takeControlOfSetAsideAsset` bertie)
      backToTheVale <- getSetAsideCard Agendas.backToTheVale
      push $ SetCurrentActDeck 1 []
      push $ SetCurrentAgendaDeck 1 [backToTheVale]
      selectEach AnyAct (toDiscard attrs)
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        select $ ConnectedFrom NotForMovement (locationWithInvestigator iid) <> UnrevealedLocation
      chooseTargetM iid locations $ lookAtRevealed iid (attrs.ability 1)
      pure a
    _ -> WheresBertie <$> liftRunMessage msg attrs
