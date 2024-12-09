module Arkham.Treachery.Cards.BeneathTheLodge (beneathTheLodge, BeneathTheLodge (..)) where

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Prelude
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BeneathTheLodge = BeneathTheLodge TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beneathTheLodge :: TreacheryCard BeneathTheLodge
beneathTheLodge = treachery BeneathTheLodge Cards.beneathTheLodge

instance HasModifiersFor BeneathTheLodge where
  getModifiersFor (BeneathTheLodge attrs) = do
    hasKey <- maybe (pure False) (fieldMap InvestigatorKeys notNull) (treacheryOwner attrs)
    self <- modifySelf attrs $ [AddKeyword Keyword.Peril | hasKey]
    skillTest <-
      getSkillTest >>= \case
        Nothing -> pure mempty
        Just st -> maybeModified_ attrs (SkillTestTarget st.id) do
          source <- MaybeT getSkillTestSource
          guard $ isSource attrs source
          pure [Difficulty 1 | hasKey]
    pure $ self <> skillTest

instance RunMessage BeneathTheLodge where
  runMessage msg t@(BeneathTheLodge attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ n | isSource attrs source -> do
      push $ HandlePointOfFailure iid (toTarget attrs) n
      pure t
    HandlePointOfFailure _ target 0 | isTarget attrs target -> pure t
    HandlePointOfFailure iid target n | isTarget attrs target -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      player <- getPlayer iid
      pushAll
        $ if hasClues
          then
            [ chooseOne
                player
                [ Label "Lose 1 clue" [RemoveClues (toSource attrs) (toTarget iid) 1]
                , Label "Take 1 horror" [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1]
                ]
            , HandlePointOfFailure iid (toTarget attrs) (n - 1)
            ]
          else
            [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
            , HandlePointOfFailure iid (toTarget attrs) (n - 1)
            ]
      pure t
    _ -> BeneathTheLodge <$> runMessage msg attrs
