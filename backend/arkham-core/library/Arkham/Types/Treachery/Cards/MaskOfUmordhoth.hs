module Arkham.Types.Treachery.Cards.MaskOfUmordhoth where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype MaskOfUmordhoth = MaskOfUmordhoth TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskOfUmordhoth :: TreacheryCard MaskOfUmordhoth
maskOfUmordhoth = treachery MaskOfUmordhoth Cards.maskOfUmordhoth

instance HasSet UniqueEnemyId env () => HasModifiersFor env MaskOfUmordhoth where
  getModifiersFor _ (EnemyTarget eid) (MaskOfUmordhoth attrs)
    | treacheryOnEnemy eid attrs = do
      uniqueEnemyIds <- map unUniqueEnemyId <$> getSetList ()
      let
        keyword =
          if eid `elem` uniqueEnemyIds then Keyword.Retaliate else Keyword.Aloof
      pure $ toModifiers attrs [HealthModifier 2, AddKeyword keyword]
  getModifiersFor _ _ _ = pure []

instance HasActions env MaskOfUmordhoth where
  getActions i window (MaskOfUmordhoth attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env MaskOfUmordhoth where
  runMessage msg t@(MaskOfUmordhoth attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      enemies <- map unFarthestEnemyId <$> getSetList (iid, EnemyTrait Cultist)
      t <$ case enemies of
        [] -> unshiftMessages
          [ FindAndDrawEncounterCard
            iid
            (CardMatchByType (EnemyType, singleton Cultist))
          , Revelation iid source
          ]
        [eid] -> unshiftMessage (AttachTreachery treacheryId (EnemyTarget eid))
        eids -> unshiftMessage
          (chooseOne
            iid
            [ AttachTreachery treacheryId (EnemyTarget eid) | eid <- eids ]
          )
    _ -> MaskOfUmordhoth <$> runMessage msg attrs
