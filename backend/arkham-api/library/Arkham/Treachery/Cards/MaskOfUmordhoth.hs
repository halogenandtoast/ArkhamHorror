module Arkham.Treachery.Cards.MaskOfUmordhoth where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype MaskOfUmordhoth = MaskOfUmordhoth TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskOfUmordhoth :: TreacheryCard MaskOfUmordhoth
maskOfUmordhoth = treachery MaskOfUmordhoth Cards.maskOfUmordhoth

instance HasModifiersFor MaskOfUmordhoth where
  getModifiersFor (MaskOfUmordhoth attrs) = case attrs.placement of
    AttachedToEnemy eid -> do
      isUnique <- elem eid <$> select UniqueEnemy
      let keyword = if isUnique then Keyword.Retaliate else Keyword.Aloof
      modified_ attrs eid [HealthModifier 2, AddKeyword keyword]
    _ -> pure mempty

instance RunMessage MaskOfUmordhoth where
  runMessage msg t@(MaskOfUmordhoth attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      enemies <- select $ FarthestEnemyFrom iid $ EnemyWithTrait Cultist
      case enemies of
        [] ->
          pushAll
            [ findAndDrawEncounterCard iid $ CardWithType EnemyType <> CardWithTrait Cultist
            , Revelation iid source
            ]
        eids -> do
          player <- getPlayer iid
          push $ chooseOrRunOne player [targetLabel eid [attachTreachery treacheryId eid] | eid <- eids]
      pure t
    _ -> MaskOfUmordhoth <$> runMessage msg attrs
