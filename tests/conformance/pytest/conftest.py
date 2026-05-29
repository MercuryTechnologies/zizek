import pytest
import pytest_subtests

# pytest-subtests 0.14.x exports SubTests but does not add it to the pytest
# namespace; hegel.conformance's type annotation requires pytest.Subtests.
if not hasattr(pytest, "Subtests"):
    pytest.Subtests = pytest_subtests.SubTests  # type: ignore[attr-defined]

pytest.register_assert_rewrite("hegel.conformance")
