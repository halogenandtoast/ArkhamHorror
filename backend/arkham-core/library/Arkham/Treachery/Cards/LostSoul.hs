module Arkham.Treachery.Cards.LostSoul
  ( lostSoul
  , LostSoul(..)
  )
where

import Arkham.Prelude

import Arkham.Campaigns.ThePathToCarcosa.Helpers
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Target
import Arkham.SkillType
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype LostSoul = LostSoul TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostSoul :: TreacheryCard LostSoul
lostSoul = treachery LostSoul Cards.lostSoul

instance TreacheryRunner env => RunMessage LostSoul where
  runMessage msg t@(LostSoul attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      moreConvictionThanDoubt <- getMoreConvictionThanDoubt
      skillTestMsg <- if moreConvictionThanDoubt
         then do
           intellect <- getSkillValue SkillIntellect iid
           pure $ RevelationSkillTest iid source SkillWillpower intellect
         else do
           willpower <- getSkillValue SkillWillpower iid
           pure $ RevelationSkillTest iid source SkillIntellect willpower
      t <$ pushAll [skillTestMsg, Discard $ toTarget attrs]
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _ -> t <$ push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0)
    _ -> LostSoul <$> runMessage msg attrs
