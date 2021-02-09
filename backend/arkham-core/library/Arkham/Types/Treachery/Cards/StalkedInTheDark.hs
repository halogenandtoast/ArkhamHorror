module Arkham.Types.Treachery.Cards.StalkedInTheDark
  ( stalkedInTheDark
  , StalkedInTheDark(..)
  )
where


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype StalkedInTheDark = StalkedInTheDark TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkedInTheDark :: TreacheryId -> a -> StalkedInTheDark
stalkedInTheDark uuid _ = StalkedInTheDark $ baseAttrs uuid "02143"

instance HasModifiersFor env StalkedInTheDark where
  getModifiersFor = noModifiersFor

instance HasActions env StalkedInTheDark where
  getActions i window (StalkedInTheDark attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env StalkedInTheDark where
  runMessage msg t@(StalkedInTheDark attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      mHuntingHorrorId <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      case mHuntingHorrorId of
        Just eid -> do
          lid <- getId @LocationId iid
          iids <- getSetList @InvestigatorId lid
          t <$ unshiftMessages
            ([Ready (EnemyTarget eid), EnemyEngageInvestigator eid iid]
            <> [ EnemyAttack iid' eid | iid' <- iids ]
            )
        Nothing ->
          t <$ unshiftMessages [Surge iid source, Discard $ toTarget attrs]
    _ -> StalkedInTheDark <$> runMessage msg attrs
